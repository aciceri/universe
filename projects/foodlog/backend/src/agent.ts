import OpenAI from "openai";
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { TOOL_DEFINITIONS, executeTool } from "./tools.js";
import {
  appendSessionMessage,
  getSessionStoredMessages,
  saveMealWithFoods,
  type StoredMessage,
  type StoredContentPart,
} from "./db.js";

const __dirname = dirname(fileURLToPath(import.meta.url));
const SYSTEM_PROMPT_TEMPLATE = readFileSync(join(__dirname, "prompt.txt"), "utf-8");

const MODEL = process.env.OPENROUTER_MODEL || "deepseek/deepseek-v4-flash";
// Keep the last N stored messages when resuming, to bound context size.
const MAX_HISTORY = 50;
const SAVE_MEAL_TOOL = "save_meal";

// Resolve the images directory from DATABASE_PATH (same parent dir as the DB).
const DATA_DIR = process.env.DATABASE_PATH
  ? join(process.env.DATABASE_PATH, "..")
  : ".";
const IMAGES_DIR = join(DATA_DIR, "images");

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

export interface PendingMealConfirmation {
  toolInput: Record<string, unknown>;
  resolve: (result: { behavior: "allow" | "deny"; message?: string }) => void;
}

export type SSEEvent =
  | { type: "text"; content: string }
  | { type: "confirm"; mealData: Record<string, unknown> }
  | { type: "done"; sessionId: string }
  | { type: "error"; message: string };

// ---------------------------------------------------------------------------
// Message helpers
// ---------------------------------------------------------------------------

/**
 * Expand file_ref parts to base64 image_url so the API receives real data.
 * Parts that are already text or image_url pass through unchanged.
 */
function expandContent(
  content: StoredMessage["content"]
): OpenAI.ChatCompletionContentPart[] | string | null {
  if (typeof content === "string" || content === null) return content;

  return content.map((part): OpenAI.ChatCompletionContentPart => {
    if (part.type === "file_ref") {
      try {
        const filePath = join(IMAGES_DIR, part.filename);
        const buffer = readFileSync(filePath);
        const ext = part.filename.split(".").pop() ?? "jpg";
        const mime =
          ext === "png" ? "image/png" : ext === "webp" ? "image/webp" : "image/jpeg";
        return {
          type: "image_url",
          image_url: { url: `data:${mime};base64,${buffer.toString("base64")}` },
        };
      } catch {
        // Image file missing — replace with a text notice so the model context stays valid.
        return { type: "text", text: "[immagine non disponibile]" };
      }
    }
    // Cast is safe: StoredContentPart text/image_url are strict subsets of the OpenAI type.
    return part as OpenAI.ChatCompletionContentPart;
  });
}

/**
 * Convert stored messages to the OpenAI API format, expanding file_refs.
 */
function toApiMessages(
  stored: StoredMessage[]
): OpenAI.ChatCompletionMessageParam[] {
  return stored.map((m) => {
    const content = expandContent(m.content);
    if (m.role === "tool") {
      return {
        role: "tool",
        content: typeof content === "string" ? content : JSON.stringify(content),
        tool_call_id: m.tool_call_id!,
      };
    }
    if (m.role === "assistant") {
      return {
        role: "assistant",
        content: typeof content === "string" ? content : null,
        ...(m.tool_calls ? { tool_calls: m.tool_calls } : {}),
      } as OpenAI.ChatCompletionAssistantMessageParam;
    }
    return {
      role: "user",
      content: content ?? "",
    } as OpenAI.ChatCompletionUserMessageParam;
  });
}

// ---------------------------------------------------------------------------
// Main agent entry point
// ---------------------------------------------------------------------------

export async function runAgent(options: {
  message: string;
  /** Base64 image uploaded in this turn (not yet saved to disk). */
  image?: { base64: string; mediaType: string };
  /** Filename already saved to disk (supplied by routes.ts after writing). */
  savedImageFilename?: string;
  sessionId?: string;
  onEvent: (event: SSEEvent) => Promise<void>;
  onConfirmationNeeded: (confirmation: PendingMealConfirmation) => void;
}): Promise<void> {
  const { message, image, savedImageFilename, sessionId, onEvent, onConfirmationNeeded } =
    options;

  const client = new OpenAI({
    baseURL: "https://openrouter.ai/api/v1",
    apiKey: process.env.OPENROUTER_API_KEY ?? "",
    defaultHeaders: {
      "HTTP-Referer": "https://github.com/aciceri/foodlog",
      "X-Title": "FoodLog",
    },
  });

  const currentTime = new Date().toLocaleString("it-IT", {
    timeZone: "Europe/Rome",
    dateStyle: "full",
    timeStyle: "medium",
  });
  const systemPrompt = SYSTEM_PROMPT_TEMPLATE.replace("{CURRENT_TIME}", currentTime);

  const activeSessionId = sessionId ?? crypto.randomUUID();

  // Load history (capped to avoid unbounded context growth).
  const storedHistory = getSessionStoredMessages(activeSessionId).slice(-MAX_HISTORY);

  // Build the new user message for storage — replace base64 with a file_ref.
  const userContent: StoredContentPart[] = [];
  if (image && savedImageFilename) {
    userContent.push({ type: "file_ref", filename: savedImageFilename });
  } else if (image) {
    // Fallback: no filename yet — embed inline (shouldn't happen in normal flow).
    userContent.push({
      type: "image_url",
      image_url: { url: `data:${image.mediaType};base64,${image.base64}` },
    });
  }
  userContent.push({ type: "text", text: message || "Analizza questa immagine" });

  const userMsg: StoredMessage = {
    role: "user",
    content: userContent.length === 1 ? userContent[0].text as string : userContent,
  };
  appendSessionMessage(activeSessionId, userMsg);
  storedHistory.push(userMsg);

  try {
    // Tool loop: keep iterating as long as the model returns tool calls.
    while (true) {
      const apiMessages: OpenAI.ChatCompletionMessageParam[] = [
        { role: "system", content: systemPrompt },
        ...toApiMessages(storedHistory),
      ];

      const stream = await client.chat.completions.create({
        model: MODEL,
        messages: apiMessages,
        tools: TOOL_DEFINITIONS,
        tool_choice: "auto",
        stream: true,
      });

      let assistantText = "";
      // Accumulate streamed tool-call fragments keyed by index.
      const tcAcc: Record<
        number,
        { id: string; type: "function"; function: { name: string; arguments: string } }
      > = {};

      for await (const chunk of stream) {
        const delta = chunk.choices[0]?.delta;
        if (!delta) continue;

        if (delta.content) {
          assistantText += delta.content;
          await onEvent({ type: "text", content: delta.content });
        }

        if (delta.tool_calls) {
          for (const tc of delta.tool_calls) {
            if (!tcAcc[tc.index]) {
              tcAcc[tc.index] = {
                id: tc.id ?? "",
                type: "function",
                function: { name: tc.function?.name ?? "", arguments: "" },
              };
            }
            if (tc.id) tcAcc[tc.index].id = tc.id;
            if (tc.function?.name) tcAcc[tc.index].function.name = tc.function.name;
            if (tc.function?.arguments) {
              tcAcc[tc.index].function.arguments += tc.function.arguments;
            }
          }
        }
      }

      const toolCalls = Object.values(tcAcc);

      // Persist assistant turn.
      const assistantMsg: StoredMessage = {
        role: "assistant",
        content: assistantText || null,
        ...(toolCalls.length > 0 ? { tool_calls: toolCalls } : {}),
      };
      appendSessionMessage(activeSessionId, assistantMsg);
      storedHistory.push(assistantMsg);

      // No tool calls → model gave a final answer, we're done.
      if (toolCalls.length === 0) break;

      // Execute each tool call and collect results.
      for (const tc of toolCalls) {
        let result: unknown;

        if (tc.function.name === SAVE_MEAL_TOOL) {
          const toolInput = JSON.parse(tc.function.arguments) as Record<string, unknown>;

          // Tell the frontend to show the confirmation card.
          await onEvent({ type: "confirm", mealData: toolInput });

          // Block the loop until the user confirms or rejects.
          const decision = await new Promise<{ behavior: "allow" | "deny"; message?: string }>(
            (resolve) => onConfirmationNeeded({ toolInput, resolve })
          );

          if (decision.behavior === "allow") {
            type FoodArg = Record<string, unknown>;
            const foods = ((toolInput.foods as FoodArg[]) ?? []).map((f) => ({
              name: f.name as string,
              calories: f.calories as number | undefined,
              protein: f.protein as number | undefined,
              carbs: f.carbs as number | undefined,
              fats: f.fats as number | undefined,
              fiber: f.fiber as number | undefined,
              sugars: f.sugars as number | undefined,
              notes: f.notes as string | undefined,
            }));
            const saved = saveMealWithFoods(
              toolInput.meal_type as string,
              foods,
              toolInput.date as string | undefined
            );
            result = { success: true, mealId: saved.mealId, foodIds: saved.foodIds };
          } else {
            result = {
              success: false,
              message: decision.message ?? "L'utente ha rifiutato il salvataggio del pasto.",
            };
          }
        } else {
          try {
            const args = JSON.parse(tc.function.arguments) as Record<string, unknown>;
            result = await executeTool(tc.function.name, args);
          } catch (err) {
            result = { error: String(err) };
          }
        }

        const toolMsg: StoredMessage = {
          role: "tool",
          content: JSON.stringify(result),
          tool_call_id: tc.id,
        };
        appendSessionMessage(activeSessionId, toolMsg);
        storedHistory.push(toolMsg);
      }
    }

    await onEvent({ type: "done", sessionId: activeSessionId });
  } catch (error) {
    const msg = error instanceof Error ? error.message : String(error);
    await onEvent({ type: "error", message: msg });
  }
}
