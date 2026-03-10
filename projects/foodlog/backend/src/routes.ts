import { Hono } from "hono";
import { streamSSE } from "hono/streaming";
import { getSessionMessages } from "@anthropic-ai/claude-agent-sdk";
import { mkdirSync, existsSync, readFileSync, writeFileSync } from "fs";
import { join } from "path";
import { getMeals, deleteFood, saveChatImage, getChatImages } from "./db.js";
import { runAgent, type PendingMealConfirmation } from "./agent.js";

const DATA_DIR = process.env.DATABASE_PATH
  ? join(process.env.DATABASE_PATH, "..")
  : ".";
const IMAGES_DIR = join(DATA_DIR, "images");

// Ensure images directory exists
mkdirSync(IMAGES_DIR, { recursive: true });

const api = new Hono();

/**
 * Active chat sessions waiting for meal confirmation.
 * Key: a unique request ID generated per chat request.
 */
const pendingConfirmations = new Map<string, PendingMealConfirmation>();

/**
 * POST /api/chat
 *
 * Accepts a chat message (and optional image), streams back SSE events.
 * Events:
 *   - { event: "request_id", data: { requestId } }  Request ID for confirmation
 *   - { event: "text", data: { content } }           Assistant text chunk
 *   - { event: "confirm", data: { mealData } }       Meal needs confirmation
 *   - { event: "done", data: { sessionId } }         Conversation turn complete
 *   - { event: "error", data: { message } }          Error occurred
 */
api.post("/api/chat", async (c) => {
  const body = await c.req.json<{
    message: string;
    session_id?: string;
    image?: { base64: string; media_type: string };
  }>();

  const requestId = crypto.randomUUID();

  // Save uploaded image to filesystem if present
  let savedImageFilename: string | undefined;
  if (body.image) {
    const ext = body.image.media_type.split("/")[1] || "jpg";
    savedImageFilename = `${crypto.randomUUID()}.${ext}`;
    const imagePath = join(IMAGES_DIR, savedImageFilename);
    const buffer = Buffer.from(body.image.base64, "base64");
    writeFileSync(imagePath, buffer);
  }

  return streamSSE(c, async (stream) => {
    // Send the request ID so the frontend can use it for confirmation
    await stream.writeSSE({
      event: "request_id",
      data: JSON.stringify({ requestId }),
      id: requestId,
    });

    // Track the resolved session ID to save image association
    let resolvedSessionId: string | undefined;

    await runAgent({
      message: body.message,
      image: body.image
        ? { base64: body.image.base64, mediaType: body.image.media_type }
        : undefined,
      sessionId: body.session_id,
      onEvent: async (event) => {
        await stream.writeSSE({
          event: event.type,
          data: JSON.stringify(event),
          id: requestId,
        });

        // Capture session ID and save image association
        if (event.type === "done" && event.sessionId) {
          resolvedSessionId = event.sessionId;

          if (savedImageFilename) {
            // Use the count of already-saved images as the index
            // (0 for first image, 1 for second, etc.)
            const existingImages = getChatImages(event.sessionId);
            saveChatImage(
              event.sessionId,
              existingImages.length,
              savedImageFilename,
            );
          }
        }
      },
      onConfirmationNeeded: (confirmation) => {
        pendingConfirmations.set(requestId, confirmation);
      },
    });

    // Clean up in case confirmation was never resolved
    pendingConfirmations.delete(requestId);
  });
});

/**
 * POST /api/chat/confirm
 *
 * Confirm or reject a pending meal save.
 * Body: { request_id: string, confirm: boolean }
 */
api.post("/api/chat/confirm", async (c) => {
  const body = await c.req.json<{
    request_id: string;
    confirm: boolean;
  }>();

  const pending = pendingConfirmations.get(body.request_id);
  if (!pending) {
    return c.json(
      { error: "No pending confirmation found for this request" },
      404,
    );
  }

  pendingConfirmations.delete(body.request_id);

  if (body.confirm) {
    pending.resolve({ behavior: "allow", updatedInput: pending.toolInput });
  } else {
    pending.resolve({
      behavior: "deny",
      message: "L'utente ha rifiutato il salvataggio del pasto.",
    });
  }

  return c.json({ success: true });
});

/**
 * GET /api/images/:filename
 *
 * Serve uploaded chat images from the filesystem.
 */
api.get("/api/images/:filename", (c) => {
  const filename = c.req.param("filename");

  // Prevent directory traversal
  if (filename.includes("/") || filename.includes("..")) {
    return c.json({ error: "Invalid filename" }, 400);
  }

  const imagePath = join(IMAGES_DIR, filename);
  if (!existsSync(imagePath)) {
    return c.json({ error: "Image not found" }, 404);
  }

  const buffer = readFileSync(imagePath);
  const ext = filename.split(".").pop() || "jpg";
  const mimeTypes: Record<string, string> = {
    jpg: "image/jpeg",
    jpeg: "image/jpeg",
    png: "image/png",
    webp: "image/webp",
    gif: "image/gif",
    heic: "image/heic",
  };

  return new Response(buffer, {
    headers: {
      "Content-Type": mimeTypes[ext] || "application/octet-stream",
      "Cache-Control": "public, max-age=31536000, immutable",
    },
  });
});

/**
 * GET /api/session/:id/messages
 *
 * Retrieve historical messages from a session via the Claude Agent SDK.
 * Returns user and assistant messages for restoring chat on page reload.
 * User messages include image URLs if photos were uploaded.
 */
api.get("/api/session/:id/messages", async (c) => {
  const sessionId = c.req.param("id");

  try {
    const messages = await getSessionMessages(sessionId);

    // Get image associations for this session (ordered by index)
    const images = getChatImages(sessionId);
    // Queue of images to assign to visible user messages in order
    const imageQueue = images.map((img) => img.filename);
    let nextImageIdx = 0;

    const chatMessages = messages
      .map((msg) => {
        const sdkMessage = msg.message as {
          role: string;
          content: string | Array<{ type: string; text?: string }>;
        };

        // Extract text from content (can be string or array of content blocks)
        let text = "";
        if (typeof sdkMessage.content === "string") {
          text = sdkMessage.content;
        } else if (Array.isArray(sdkMessage.content)) {
          text = sdkMessage.content
            .filter((block) => block.type === "text" && block.text)
            .map((block) => block.text!)
            .join("\n");
        }

        // Skip empty messages (e.g. tool-only turns)
        if (!text.trim()) return null;

        // For visible user messages, check if there's an image
        // (images sent by the user are matched in chronological order)
        let image: string | undefined;
        if (msg.type === "user") {
          // Check if the SDK message had image content blocks
          const hasImageContent =
            Array.isArray(sdkMessage.content) &&
            sdkMessage.content.some((block) => block.type === "image");
          if (hasImageContent && nextImageIdx < imageQueue.length) {
            image = `/api/images/${imageQueue[nextImageIdx]}`;
            nextImageIdx++;
          }
        }

        return {
          role: msg.type as "user" | "assistant",
          content: text,
          image,
        };
      })
      .filter(Boolean);

    return c.json({ messages: chatMessages });
  } catch (error) {
    const errorMessage =
      error instanceof Error ? error.message : String(error);
    return c.json({ messages: [], error: errorMessage }, 200);
  }
});

/**
 * GET /api/meals?from=YYYY-MM-DD&to=YYYY-MM-DD
 *
 * Get meals for a date range. Direct SQLite query, no agent needed.
 */
api.get("/api/meals", (c) => {
  const from = c.req.query("from");
  const to = c.req.query("to");

  if (!from || !to) {
    return c.json({ error: "Missing 'from' and 'to' query parameters" }, 400);
  }

  const meals = getMeals(from, to);
  return c.json({ meals });
});

/**
 * DELETE /api/foods/:id
 *
 * Delete a food item. Direct SQLite operation, no agent needed.
 */
api.delete("/api/foods/:id", (c) => {
  const foodId = parseInt(c.req.param("id"), 10);

  if (isNaN(foodId)) {
    return c.json({ error: "Invalid food ID" }, 400);
  }

  const deleted = deleteFood(foodId);

  if (!deleted) {
    return c.json({ error: "Food not found" }, 404);
  }

  return c.json({ success: true, message: "Alimento cancellato con successo" });
});

export { api };
