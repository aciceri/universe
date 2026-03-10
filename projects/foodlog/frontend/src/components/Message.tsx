import Markdown from "react-markdown";
import type { ChatMessage } from "../services/api";
import { User, Bot } from "lucide-react";

interface MessageProps {
  message: ChatMessage;
}

export function Message({ message }: MessageProps) {
  const isUser = message.role === "user";

  return (
    <div
      className={`flex gap-3 p-4 ${isUser ? "flex-row-reverse" : "flex-row"}`}
    >
      <div
        className={`flex-shrink-0 w-8 h-8 rounded-full flex items-center justify-center ${
          isUser
            ? "bg-ctp-mauve text-ctp-base"
            : "bg-ctp-surface0 text-ctp-text"
        }`}
      >
        {isUser ? <User size={18} /> : <Bot size={18} />}
      </div>

      <div
        className={`max-w-[75%] rounded-2xl px-4 py-2 ${
          isUser
            ? "bg-ctp-mauve text-ctp-base"
            : "bg-ctp-surface0 text-ctp-text"
        }`}
      >
        {/* Show image if present (user messages with photos) */}
        {message.image && (
          <img
            src={message.image}
            alt="Foto cibo"
            className="max-h-48 rounded-lg mb-2 object-cover"
          />
        )}

        {message.content &&
          (isUser ? (
            <p className="text-sm whitespace-pre-wrap break-words">
              {message.content}
            </p>
          ) : (
            <div
              className="text-sm prose prose-sm prose-invert max-w-none break-words
                [&>p]:my-1 [&>ul]:my-1 [&>ol]:my-1 [&>li]:my-0.5
                [&>p:first-child]:mt-0 [&>p:last-child]:mb-0
                [&_strong]:text-ctp-text [&_em]:text-ctp-subtext1
                [&_ul]:list-disc [&_ul]:pl-4 [&_ol]:list-decimal [&_ol]:pl-4
                [&_code]:bg-ctp-mantle [&_code]:px-1 [&_code]:rounded [&_code]:text-xs"
            >
              <Markdown>{message.content}</Markdown>
            </div>
          ))}
      </div>
    </div>
  );
}
