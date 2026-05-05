import { useState, useCallback, useEffect, useRef } from "react";
import type { ChatMessage, MealSummary } from "../services/api";
import {
  sendChatMessage,
  confirmMeal,
  getSessionMessages,
  getCurrentSession,
  resetSession,
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

export function ChatContainer({ onMealConfirmed }: ChatContainerProps) {
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [pendingConfirmation, setPendingConfirmation] =
    useState<MealSummary | null>(null);
  const [confirmedMeal, setConfirmedMeal] = useState<MealSummary | null>(null);
  const [rejectedMeals, setRejectedMeals] = useState<RejectedMeal[]>([]);

  const currentRequestId = useRef<string | null>(null);
  // Session ID is always fetched from the server — shared across devices.
  const sessionId = useRef<string | undefined>(undefined);

  // On mount: fetch canonical session ID from server, then restore history.
  useEffect(() => {
    let cancelled = false;
    setIsLoading(true);

    getCurrentSession()
      .then((id) => {
        if (cancelled) return;
        sessionId.current = id;
        return getSessionMessages(id);
      })
      .then((restored) => {
        if (cancelled || !restored) return;
        if (restored.length > 0) setMessages(restored);
      })
      .catch((err) => {
        console.warn("Failed to initialise session:", err);
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

      if (pendingConfirmation) {
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
                setIsLoading(false);
                break;

              case "done":
                // Server echoes back the session ID — keep our ref in sync.
                if (event.sessionId) sessionId.current = event.sessionId;
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
      setConfirmedMeal(pendingConfirmation);
      setPendingConfirmation(null);
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

  const handleNewSession = useCallback(async () => {
    try {
      const newId = await resetSession();
      sessionId.current = newId;
    } catch (err) {
      console.error("Failed to reset session:", err);
    }
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
