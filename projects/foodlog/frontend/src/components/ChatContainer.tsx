import { useState } from "react";
import type { ChatMessage, MealSummary } from "../services/api";
import { sendMessage } from "../services/api";
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

  const handleSendMessage = async (content: string) => {
    const now = new Date();

    // If there's a pending confirmation that wasn't accepted, mark it as rejected
    // with timestamp BEFORE the user's new message
    if (pendingConfirmation) {
      setRejectedMeals((prev) => [
        ...prev,
        {
          meal: pendingConfirmation,
          rejectedAt: now,
        },
      ]);
    }

    const userMessage: ChatMessage = {
      role: "user",
      content,
      timestamp: now,
    };

    setMessages((prev) => [...prev, userMessage]);
    setIsLoading(true);
    setPendingConfirmation(null);
    setConfirmedMeal(null);

    try {
      // Send last 6 messages as context to maintain conversation flow
      const recentMessages = messages.slice(-6);
      const response = await sendMessage(
        content,
        false,
        undefined,
        recentMessages,
      );

      const assistantMessage: ChatMessage = {
        role: "assistant",
        content: response.message,
        timestamp: new Date(),
        summary: response.summary,
      };

      setMessages((prev) => [...prev, assistantMessage]);

      // If LLM response requires confirmation, save it for later
      if (response.needs_confirmation && response.summary) {
        setPendingConfirmation(response.summary);
      }
    } catch (error) {
      console.error("Error sending message:", error);

      const errorMessage: ChatMessage = {
        role: "assistant",
        content: "Scusa, c'è stato un errore. Riprova!",
        timestamp: new Date(),
      };

      setMessages((prev) => [...prev, errorMessage]);
    } finally {
      setIsLoading(false);
    }
  };

  const handleConfirm = async () => {
    if (!pendingConfirmation) return;

    setIsLoading(true);

    try {
      const response = await sendMessage("", true, pendingConfirmation);

      const confirmMessage: ChatMessage = {
        role: "assistant",
        content: response.message,
        timestamp: new Date(),
      };

      setMessages((prev) => [...prev, confirmMessage]);

      // Move from pending to confirmed
      setConfirmedMeal(pendingConfirmation);
      setPendingConfirmation(null);

      // Notify parent component that a meal was confirmed
      if (onMealConfirmed) {
        onMealConfirmed();
      }
    } catch (error) {
      console.error("Error confirming:", error);

      const errorMessage: ChatMessage = {
        role: "assistant",
        content: "Errore nel salvare il pasto. Riprova!",
        timestamp: new Date(),
      };

      setMessages((prev) => [...prev, errorMessage]);
    } finally {
      setIsLoading(false);
    }
  };

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

      <ChatInput onSend={handleSendMessage} disabled={isLoading} />
    </div>
  );
}
