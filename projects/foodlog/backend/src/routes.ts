import { Hono } from "hono";
import { streamSSE } from "hono/streaming";
import { mkdirSync, existsSync, readFileSync, writeFileSync } from "fs";
import { join } from "path";
import {
  getMeals,
  deleteFood,
  saveChatImage,
  getChatImages,
  getSessionStoredMessages,
  sessionExists,
  getCurrentSessionId,
  resetCurrentSession,
  type StoredContentPart,
} from "./db.js";
import { runAgent, type PendingMealConfirmation } from "./agent.js";

const DATA_DIR = process.env.DATABASE_PATH
  ? join(process.env.DATABASE_PATH, "..")
  : ".";
const IMAGES_DIR = join(DATA_DIR, "images");

mkdirSync(IMAGES_DIR, { recursive: true });

const api = new Hono();

/**
 * Active chat sessions waiting for meal confirmation.
 * Key: a unique request ID generated per chat request.
 */
const pendingConfirmations = new Map<string, PendingMealConfirmation>();

// ---------------------------------------------------------------------------
// POST /api/chat
// ---------------------------------------------------------------------------

api.post("/api/chat", async (c) => {
  const body = await c.req.json<{
    message: string;
    session_id?: string;
    image?: { base64: string; media_type: string };
  }>();

  const requestId = crypto.randomUUID();

  // Save uploaded image to disk before starting the agent so we can pass
  // the filename (not the raw base64) into runAgent for lean DB storage.
  let savedImageFilename: string | undefined;
  if (body.image) {
    const ext = body.image.media_type.split("/")[1] || "jpg";
    savedImageFilename = `${crypto.randomUUID()}.${ext}`;
    const buffer = Buffer.from(body.image.base64, "base64");
    writeFileSync(join(IMAGES_DIR, savedImageFilename), buffer);
  }

  return streamSSE(c, async (stream) => {
    await stream.writeSSE({
      event: "request_id",
      data: JSON.stringify({ requestId }),
      id: requestId,
    });

    let resolvedSessionId: string | undefined;

    await runAgent({
      message: body.message,
      image: body.image
        ? { base64: body.image.base64, mediaType: body.image.media_type }
        : undefined,
      savedImageFilename,
      sessionId: body.session_id,
      onEvent: async (event) => {
        await stream.writeSSE({
          event: event.type,
          data: JSON.stringify(event),
          id: requestId,
        });

        if (event.type === "done") {
          resolvedSessionId = event.sessionId;

          // Record image → session association for chat restore.
          if (savedImageFilename && resolvedSessionId) {
            const existing = getChatImages(resolvedSessionId);
            saveChatImage(resolvedSessionId, existing.length, savedImageFilename);
          }
        }
      },
      onConfirmationNeeded: (confirmation) => {
        pendingConfirmations.set(requestId, confirmation);
      },
    });

    pendingConfirmations.delete(requestId);
  });
});

// ---------------------------------------------------------------------------
// POST /api/chat/confirm
// ---------------------------------------------------------------------------

api.post("/api/chat/confirm", async (c) => {
  const body = await c.req.json<{ request_id: string; confirm: boolean }>();

  const pending = pendingConfirmations.get(body.request_id);
  if (!pending) {
    return c.json({ error: "No pending confirmation found for this request" }, 404);
  }

  pendingConfirmations.delete(body.request_id);

  if (body.confirm) {
    pending.resolve({ behavior: "allow" });
  } else {
    pending.resolve({
      behavior: "deny",
      message: "L'utente ha rifiutato il salvataggio del pasto.",
    });
  }

  return c.json({ success: true });
});

// ---------------------------------------------------------------------------
// GET /api/images/:filename
// ---------------------------------------------------------------------------

api.get("/api/images/:filename", (c) => {
  const filename = c.req.param("filename");

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

// ---------------------------------------------------------------------------
// GET /api/session/:id/messages
// ---------------------------------------------------------------------------

/**
 * Restore chat history for display in the frontend.
 * We read from our own session_message table and strip out tool-only turns,
 * attaching image URLs from the chat_image table where applicable.
 */
api.get("/api/session/:id/messages", (c) => {
  const sessionId = c.req.param("id");

  try {
    if (!sessionExists(sessionId)) {
      return c.json({ messages: [] });
    }

    const stored = getSessionStoredMessages(sessionId);
    const images = getChatImages(sessionId);
    // Queue images in order; each user message with a file_ref consumes one.
    const imageQueue = images.map((img) => img.filename);
    let imageIdx = 0;

    const chatMessages: Array<{
      role: "user" | "assistant";
      content: string;
      image?: string;
    }> = [];

    for (const msg of stored) {
      // Skip tool-result messages — not displayable as chat bubbles.
      if (msg.role === "tool") continue;

      // Extract plain text from the content.
      let text = "";
      if (typeof msg.content === "string") {
        text = msg.content;
      } else if (Array.isArray(msg.content)) {
        text = (msg.content as StoredContentPart[])
          .filter((p): p is { type: "text"; text: string } => p.type === "text")
          .map((p) => p.text)
          .join("\n");
      }

      if (!text?.trim() && msg.role === "assistant") continue;

      // Attach image URL for user messages that included a photo.
      let imageUrl: string | undefined;
      if (msg.role === "user") {
        const hasFileRef =
          Array.isArray(msg.content) &&
          (msg.content as StoredContentPart[]).some((p) => p.type === "file_ref");
        if (hasFileRef && imageIdx < imageQueue.length) {
          imageUrl = toAbsoluteImageUrl(`/api/images/${imageQueue[imageIdx++]}`);
        }
      }

      chatMessages.push({
        role: msg.role as "user" | "assistant",
        content: text,
        ...(imageUrl ? { image: imageUrl } : {}),
      });
    }

    return c.json({ messages: chatMessages });
  } catch (error) {
    const msg = error instanceof Error ? error.message : String(error);
    return c.json({ messages: [], error: msg }, 200);
  }
});

// ---------------------------------------------------------------------------
// GET /api/meals
// ---------------------------------------------------------------------------

api.get("/api/session/current", (c) => {
  return c.json({ sessionId: getCurrentSessionId() });
});

api.post("/api/session/reset", (c) => {
  const sessionId = resetCurrentSession();
  return c.json({ sessionId });
});

api.get("/api/meals", (c) => {
  const from = c.req.query("from");
  const to = c.req.query("to");

  if (!from || !to) {
    return c.json({ error: "Missing 'from' and 'to' query parameters" }, 400);
  }

  return c.json({ meals: getMeals(from, to) });
});

// ---------------------------------------------------------------------------
// DELETE /api/foods/:id
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function toAbsoluteImageUrl(path: string): string {
  const apiBase = process.env.VITE_API_URL || "";
  const origin = apiBase.replace(/\/api$/, "");
  if (!origin) return path;
  return origin + path;
}

export { api };
