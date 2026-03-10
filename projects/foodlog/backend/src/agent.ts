import { query } from "@anthropic-ai/claude-agent-sdk";
import { createFoodlogMcpServer, SAVE_MEAL_TOOL_NAME } from "./tools.js";
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const SYSTEM_PROMPT_TEMPLATE = readFileSync(
  join(__dirname, "prompt.txt"),
  "utf-8",
);

const MODEL = process.env.CLAUDE_MODEL || "opus";

/**
 * Represents the data of a save_meal tool call that needs user confirmation.
 */
export interface PendingMealConfirmation {
  toolInput: Record<string, unknown>;
  resolve: (result: {
    behavior: "allow" | "deny";
    updatedInput?: Record<string, unknown>;
    message?: string;
  }) => void;
}

/**
 * Represents an SSE event sent to the frontend.
 */
export type SSEEvent =
  | { type: "text"; content: string }
  | { type: "confirm"; mealData: Record<string, unknown> }
  | { type: "done"; sessionId: string }
  | { type: "error"; message: string };

/**
 * Run the Claude agent for a chat message.
 *
 * Uses streaming input mode for full MCP + canUseTool support.
 * The onEvent callback is called for each SSE event to send to the frontend.
 * The onConfirmationNeeded callback fires when the agent blocks waiting for
 * user confirmation on a save_meal — this lets the caller store the pending
 * confirmation immediately (before runAgent returns).
 */
export async function runAgent(options: {
  message: string;
  image?: { base64: string; mediaType: string };
  sessionId?: string;
  onEvent: (event: SSEEvent) => void;
  onConfirmationNeeded: (confirmation: PendingMealConfirmation) => void;
}): Promise<void> {
  const { message, image, sessionId, onEvent, onConfirmationNeeded } = options;

  const currentTime = new Date().toLocaleString("it-IT", {
    timeZone: "Europe/Rome",
    dateStyle: "full",
    timeStyle: "medium",
  });

  const systemPrompt = SYSTEM_PROMPT_TEMPLATE.replace(
    "{CURRENT_TIME}",
    currentTime,
  );

  // Build message content (text + optional image)
  const contentParts: Array<
    | { type: "text"; text: string }
    | {
        type: "image";
        source: { type: "base64"; media_type: string; data: string };
      }
  > = [];

  if (image) {
    contentParts.push({
      type: "image",
      source: {
        type: "base64",
        media_type: image.mediaType,
        data: image.base64,
      },
    });
  }

  contentParts.push({
    type: "text",
    text: message || "Analizza questa immagine",
  });

  // Generate messages for streaming input mode
  // SDKUserMessage requires parent_tool_use_id and session_id
  async function* generateMessages() {
    yield {
      type: "user" as const,
      message: {
        role: "user" as const,
        content: contentParts,
      },
      parent_tool_use_id: null,
      session_id: sessionId || "",
    };
  }

  const queryOptions: Parameters<typeof query>[0]["options"] = {
    systemPrompt: systemPrompt,
    model: MODEL,
    mcpServers: {
      foodlog: createFoodlogMcpServer(),
    },
    allowedTools: [
      // Auto-approve read-only tools
      "mcp__foodlog__get_meals",
      "mcp__foodlog__get_daily_summary",
      "mcp__foodlog__delete_food",
    ],
    // save_meal is NOT in allowedTools — it will trigger canUseTool
    maxTurns: 10,
    // Enable partial/streaming messages for real-time text streaming
    includePartialMessages: true,
    canUseTool: async (_toolName, _input, _options) => {
      if (_toolName === SAVE_MEAL_TOOL_NAME) {
        // Send confirmation request to frontend via SSE
        onEvent({ type: "confirm", mealData: _input });

        // Block the agent and expose the resolve function to the caller
        return new Promise((resolve) => {
          const confirmation: PendingMealConfirmation = {
            toolInput: _input,
            resolve: (result) => {
              if (result.behavior === "allow") {
                resolve({
                  behavior: "allow",
                  updatedInput: result.updatedInput || _input,
                });
              } else {
                resolve({
                  behavior: "deny",
                  message:
                    result.message ||
                    "L'utente ha rifiutato il salvataggio del pasto.",
                });
              }
            },
          };
          // Notify caller immediately so they can store it for the confirm endpoint
          onConfirmationNeeded(confirmation);
        });
      }

      // Auto-approve everything else
      return { behavior: "allow" as const, updatedInput: _input };
    },
    // Required workaround for canUseTool in streaming mode:
    // a dummy PreToolUse hook that returns continue_ to keep the stream open
    hooks: {
      PreToolUse: [
        {
          matcher: undefined,
          hooks: [
            async (
              _input: unknown,
              _toolUseID: string | undefined,
              _opts: { signal: AbortSignal },
            ) => {
              return { continue: true };
            },
          ],
        },
      ],
    },
  };

  // Resume an existing session if provided
  if (sessionId) {
    (queryOptions as Record<string, unknown>).resume = sessionId;
  }

  let resolvedSessionId: string | undefined;

  try {
    const stream = query({
      prompt: generateMessages(),
      options: queryOptions,
    });

    for await (const msg of stream) {
      // Capture session ID from init message
      if (
        msg.type === "system" &&
        "subtype" in msg &&
        msg.subtype === "init"
      ) {
        resolvedSessionId = (msg as { session_id: string }).session_id;
      }

      // Handle partial/streaming text for real-time display
      if (msg.type === "stream_event") {
        const event = (msg as { event: Record<string, unknown> }).event;
        // content_block_delta with text_delta contains streaming text chunks
        if (event.type === "content_block_delta") {
          const delta = event.delta as Record<string, unknown> | undefined;
          if (
            delta?.type === "text_delta" &&
            typeof delta.text === "string"
          ) {
            onEvent({ type: "text", content: delta.text });
          }
        }
      }

      // Handle result — agent turn is complete
      if (msg.type === "result") {
        const resultMsg = msg as { subtype: string; session_id: string };
        if (resultMsg.subtype === "success") {
          onEvent({
            type: "done",
            sessionId: resolvedSessionId || resultMsg.session_id,
          });
        } else {
          onEvent({
            type: "error",
            message: `Agent error: ${resultMsg.subtype}`,
          });
        }
      }
    }
  } catch (error) {
    const errorMessage =
      error instanceof Error ? error.message : String(error);
    onEvent({ type: "error", message: errorMessage });
  }
}
