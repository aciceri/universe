import { useState, useCallback, useEffect, useRef } from "react";
import type { ChatMessage, MealSummary } from "../services/api";
import {
  sendChatMessage,
  confirmMeal,
  getSessionMessages,
} from "../services/api";
import { MessageList } from "./MessageList";
import { ChatInput } from "./ChatInput";

export interface RejectedMeal {
  meal: MealSummary;
  rejectedAt: Date;
}

interface ChatContainerProps {
  onMealConfirmed?: () => void;
}

const SESSION_KEY = "foodlog_session_id";

function getStoredSessionId(): string | undefined {
  return localStorage.getItem(SESSION_KEY) || undefined;
}

function storeSessionId(sessionId: string): void {
  localStorage.setItem(SESSION_KEY, sessionId);
}

export function ChatContainer({ onMealConfirmed }: ChatContainerProps) {
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [pendingConfirmation, setPendingConfirmation] =
    useState<MealSummary | null>(null);
  const [confirmedMeal, setConfirmedMeal] = useState<MealSummary | null>(null);
  const [rejectedMeals, setRejectedMeals] = useState<RejectedMeal[]>([]);

  // Track the current request ID for confirming meals
  const currentRequestId = useRef<string | null>(null);
  const sessionId = useRef<string | undefined>(getStoredSessionId());

  // Restore chat history from the SDK session on mount
  useEffect(() => {
    const storedId = sessionId.current;
    if (!storedId) return;

    let cancelled = false;
    setIsLoading(true);

    getSessionMessages(storedId)
      .then((restored) => {
        if (cancelled) return;
        if (restored.length > 0) {
          setMessages(restored);
        } else {
          // Session no longer exists (e.g. DB was reset) — clear stale ID
          localStorage.removeItem(SESSION_KEY);
          sessionId.current = undefined;
        }
      })
      .catch((err) => {
        console.warn("Failed to restore session messages:", err);
        if (!cancelled) {
          // Clear stale session ID so we don't try to resume a dead session
          localStorage.removeItem(SESSION_KEY);
          sessionId.current = undefined;
        }
      })
      .finally(() => {
        if (!cancelled) setIsLoading(false);
      });

    return () => {
      cancelled = true;
    };
  }, []);

  const handleSendMessage = useCallback(
    async (content: string, image?: { base64: string; mediaType: string }) => {
      const now = new Date();

      // If there's a pending confirmation that wasn't accepted, reject it
      if (pendingConfirmation) {
        // Also reject via API so the agent knows
        if (currentRequestId.current) {
          confirmMeal(currentRequestId.current, false).catch(console.error);
        }
        setRejectedMeals((prev) => [
          ...prev,
          { meal: pendingConfirmation, rejectedAt: now },
        ]);
      }

      const userMessage: ChatMessage = {
        role: "user",
        content,
        timestamp: now,
        image: image
          ? `data:${image.mediaType};base64,${image.base64}`
          : undefined,
      };

      setMessages((prev) => [...prev, userMessage]);
      setIsLoading(true);
      setPendingConfirmation(null);
      setConfirmedMeal(null);
      currentRequestId.current = null;

      // Accumulate text chunks into a single assistant message
      let assistantText = "";

      try {
        await sendChatMessage({
          message: content,
          sessionId: sessionId.current,
          image,
          onEvent: (event) => {
            switch (event.type) {
              case "request_id":
                currentRequestId.current = event.requestId;
                break;

              case "text":
                assistantText += event.content;
                // Update the assistant message in-place for streaming effect
                setMessages((prev) => {
                  const lastMsg = prev[prev.length - 1];
                  if (lastMsg?.role === "assistant") {
                    return [
                      ...prev.slice(0, -1),
                      { ...lastMsg, content: assistantText },
                    ];
                  }
                  return [
                    ...prev,
                    {
                      role: "assistant" as const,
                      content: assistantText,
                      timestamp: new Date(),
                    },
                  ];
                });
                break;

              case "confirm":
                setPendingConfirmation(event.mealData);
                // Stop loading so the confirm button is clickable
                // and the "Sto pensando..." spinner disappears.
                // The SSE stream is still open (waiting for confirmation),
                // but from the user's perspective we're waiting for their input.
                setIsLoading(false);
                break;

              case "done":
                if (event.sessionId) {
                  sessionId.current = event.sessionId;
                  storeSessionId(event.sessionId);
                }
                break;

              case "error":
                setMessages((prev) => [
                  ...prev,
                  {
                    role: "assistant" as const,
                    content: `Errore: ${event.message}`,
                    timestamp: new Date(),
                  },
                ]);
                break;
            }
          },
        });
      } catch (error) {
        console.error("Error sending message:", error);
        setMessages((prev) => [
          ...prev,
          {
            role: "assistant" as const,
            content: "Scusa, c'e' stato un errore di connessione. Riprova!",
            timestamp: new Date(),
          },
        ]);
      } finally {
        setIsLoading(false);
      }
    },
    [pendingConfirmation],
  );

  const handleConfirm = useCallback(async () => {
    if (!pendingConfirmation || !currentRequestId.current) return;

    setIsLoading(true);

    try {
      await confirmMeal(currentRequestId.current, true);

      // Move from pending to confirmed
      setConfirmedMeal(pendingConfirmation);
      setPendingConfirmation(null);

      // Notify parent that a meal was saved (refreshes stats)
      onMealConfirmed?.();
    } catch (error) {
      console.error("Error confirming:", error);
      setMessages((prev) => [
        ...prev,
        {
          role: "assistant" as const,
          content: "Errore nel salvare il pasto. Riprova!",
          timestamp: new Date(),
        },
      ]);
    } finally {
      setIsLoading(false);
    }
  }, [pendingConfirmation, onMealConfirmed]);

  const handleNewSession = useCallback(() => {
    localStorage.removeItem(SESSION_KEY);
    sessionId.current = undefined;
    setMessages([]);
    setPendingConfirmation(null);
    setConfirmedMeal(null);
    setRejectedMeals([]);
    currentRequestId.current = null;
  }, []);

  return (
    <div className="flex flex-col h-full bg-ctp-base">
      <MessageList
        messages={messages}
        isLoading={isLoading}
        pendingConfirmation={pendingConfirmation}
        confirmedMeal={confirmedMeal}
        rejectedMeals={rejectedMeals}
        onConfirm={handleConfirm}
      />

      <ChatInput
        onSend={handleSendMessage}
        onNewSession={handleNewSession}
        disabled={isLoading}
      />
    </div>
  );
}
