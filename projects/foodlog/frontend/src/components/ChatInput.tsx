import { useState } from "react";
import type { FormEvent, KeyboardEvent } from "react";
import { Send } from "lucide-react";

interface ChatInputProps {
  onSend: (message: string) => void;
  disabled: boolean;
}

export function ChatInput({ onSend, disabled }: ChatInputProps) {
  const [input, setInput] = useState("");

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    if (input.trim() && !disabled) {
      onSend(input.trim());
      setInput("");
    }
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit(e);
    }
  };

  return (
    <form
      onSubmit={handleSubmit}
      className="border-t border-ctp-surface0 bg-ctp-base p-4"
    >
      <div className="flex gap-2 items-end">
        <textarea
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="Es: Ho mangiato pasta al pomodoro..."
          disabled={disabled}
          rows={1}
          className="flex-1 resize-none rounded-2xl border border-ctp-surface0 bg-ctp-surface0 text-ctp-text placeholder-ctp-subtext0 px-4 py-3 focus:outline-none focus:ring-2 focus:ring-ctp-mauve focus:border-transparent disabled:opacity-50 disabled:cursor-not-allowed text-sm"
          style={{ maxHeight: "120px" }}
          onInput={(e) => {
            const target = e.target as HTMLTextAreaElement;
            target.style.height = "auto";
            target.style.height = target.scrollHeight + "px";
          }}
        />
        <button
          type="submit"
          disabled={!input.trim() || disabled}
          className="flex-shrink-0 w-10 h-10 rounded-full bg-ctp-mauve text-ctp-base flex items-center justify-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-ctp-mauve/80 transition-colors active:scale-95"
        >
          <Send size={18} />
        </button>
      </div>
    </form>
  );
}
