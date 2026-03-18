import { useState, useRef, useCallback } from "react";
import type { FormEvent, KeyboardEvent } from "react";
import {
  Send,
  ImagePlus,
  X,
  RotateCcw,
  Mic,
  Square,
  Loader2,
} from "lucide-react";
import { useWhisper } from "../hooks/useWhisper";

interface ChatInputProps {
  onSend: (
    message: string,
    image?: { base64: string; mediaType: string },
  ) => void;
  onNewSession: () => void;
  disabled: boolean;
}

export function ChatInput({ onSend, onNewSession, disabled }: ChatInputProps) {
  const [input, setInput] = useState("");
  const [imagePreview, setImagePreview] = useState<string | null>(null);
  const [imageData, setImageData] = useState<{
    base64: string;
    mediaType: string;
  } | null>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  // Whisper voice input
  const handleTranscription = useCallback((text: string) => {
    setInput((prev) => {
      // Append transcribed text (with space separator if there's existing text)
      const separator = prev.trim() ? " " : "";
      return prev + separator + text;
    });
    // Focus the textarea so the user can review/edit
    setTimeout(() => textareaRef.current?.focus(), 0);
  }, []);

  const {
    status: whisperStatus,
    progress,
    error: whisperError,
    startRecording,
    stopRecording,
    isSupported: whisperSupported,
  } = useWhisper(handleTranscription);

  const isRecording = whisperStatus === "recording";
  const isTranscribing = whisperStatus === "transcribing";
  const isModelLoading = whisperStatus === "loading";
  const whisperBusy = isRecording || isTranscribing || isModelLoading;

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();
    if ((!input.trim() && !imageData) || disabled) return;

    onSend(input.trim(), imageData || undefined);
    setInput("");
    setImagePreview(null);
    setImageData(null);
  };

  const handleKeyDown = (e: KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit(e);
    }
  };

  const handleImageSelect = () => {
    fileInputRef.current?.click();
  };

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;

    // Validate file type
    if (!file.type.startsWith("image/")) return;

    // Create preview URL
    const previewUrl = URL.createObjectURL(file);
    setImagePreview(previewUrl);

    // Convert to base64
    const reader = new FileReader();
    reader.onload = () => {
      const result = reader.result as string;
      // Extract base64 data (remove the data:image/...;base64, prefix)
      const base64 = result.split(",")[1];
      setImageData({ base64, mediaType: file.type });
    };
    reader.readAsDataURL(file);

    // Reset input so the same file can be selected again
    e.target.value = "";
  };

  const removeImage = () => {
    if (imagePreview) {
      URL.revokeObjectURL(imagePreview);
    }
    setImagePreview(null);
    setImageData(null);
  };

  const handleMicClick = () => {
    if (isRecording) {
      stopRecording();
    } else {
      startRecording();
    }
  };

  // Build model loading status text
  const getWhisperStatusText = () => {
    if (isModelLoading) {
      const pct = progress.percent;
      if (pct != null && pct > 0) {
        return `Caricamento modello... ${Math.round(pct)}%`;
      }
      return "Caricamento modello vocale...";
    }
    if (isTranscribing) return "Trascrizione in corso...";
    if (isRecording) return "Registrazione in corso...";
    return null;
  };

  const whisperStatusText = getWhisperStatusText();

  return (
    <form
      onSubmit={handleSubmit}
      className="border-t border-ctp-surface0 bg-ctp-base p-4 space-y-2"
    >
      {/* Whisper status bar */}
      {(whisperStatusText || whisperError) && (
        <div className="flex items-center gap-2 text-xs px-1">
          {whisperStatusText && (
            <>
              {(isModelLoading || isTranscribing) && (
                <Loader2 size={12} className="animate-spin text-ctp-mauve" />
              )}
              {isRecording && (
                <span className="w-2 h-2 rounded-full bg-ctp-red animate-pulse" />
              )}
              <span className="text-ctp-subtext0">{whisperStatusText}</span>
            </>
          )}
          {whisperError && <span className="text-ctp-red">{whisperError}</span>}
        </div>
      )}

      {/* Image preview */}
      {imagePreview && (
        <div className="relative inline-block">
          <img
            src={imagePreview}
            alt="Anteprima"
            className="h-20 rounded-lg object-cover border border-ctp-surface1"
          />
          <button
            type="button"
            onClick={removeImage}
            className="absolute -top-2 -right-2 w-6 h-6 rounded-full bg-ctp-red text-ctp-base flex items-center justify-center hover:bg-ctp-red/80"
          >
            <X size={14} />
          </button>
        </div>
      )}

      <div className="flex gap-2 items-end">
        {/* Hidden file input */}
        <input
          ref={fileInputRef}
          type="file"
          accept="image/*"
          capture="environment"
          onChange={handleFileChange}
          className="hidden"
        />

        {/* Image button */}
        <button
          type="button"
          onClick={handleImageSelect}
          disabled={disabled || whisperBusy}
          className="flex-shrink-0 w-10 h-10 rounded-full bg-ctp-surface0 text-ctp-subtext0 flex items-center justify-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-ctp-surface1 transition-colors"
          title="Aggiungi foto"
        >
          <ImagePlus size={18} />
        </button>

        {/* Mic button */}
        {whisperSupported && (
          <button
            type="button"
            onClick={handleMicClick}
            disabled={disabled || isTranscribing || isModelLoading}
            className={`flex-shrink-0 w-10 h-10 rounded-full flex items-center justify-center disabled:opacity-50 disabled:cursor-not-allowed transition-colors active:scale-95 ${
              isRecording
                ? "bg-ctp-red text-ctp-base hover:bg-ctp-red/80"
                : "bg-ctp-surface0 text-ctp-subtext0 hover:bg-ctp-surface1"
            }`}
            title={
              isRecording ? "Ferma registrazione" : "Registra messaggio vocale"
            }
          >
            {isModelLoading || isTranscribing ? (
              <Loader2 size={18} className="animate-spin" />
            ) : isRecording ? (
              <Square size={16} />
            ) : (
              <Mic size={18} />
            )}
          </button>
        )}

        {/* New session button */}
        <button
          type="button"
          onClick={onNewSession}
          disabled={disabled || whisperBusy}
          className="flex-shrink-0 w-10 h-10 rounded-full bg-ctp-surface0 text-ctp-subtext0 flex items-center justify-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-ctp-surface1 transition-colors"
          title="Nuova conversazione"
        >
          <RotateCcw size={18} />
        </button>

        <textarea
          ref={textareaRef}
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={handleKeyDown}
          placeholder="Es: Ho mangiato pasta al pomodoro..."
          disabled={disabled || whisperBusy}
          rows={1}
          className="flex-1 resize-none rounded-2xl border border-ctp-surface0 bg-ctp-surface0 text-ctp-text placeholder-ctp-subtext0 px-4 py-3 focus:outline-none focus:ring-2 focus:ring-ctp-mauve focus:border-transparent disabled:opacity-50 disabled:cursor-not-allowed text-sm"
          style={{ maxHeight: "120px" }}
          onInput={(e) => {
            const target = e.target as HTMLTextAreaElement;
            target.style.height = "auto";
            target.style.height = target.scrollHeight + "px";
          }}
        />

        {/* Send button */}
        <button
          type="submit"
          disabled={(!input.trim() && !imageData) || disabled || whisperBusy}
          className="flex-shrink-0 w-10 h-10 rounded-full bg-ctp-mauve text-ctp-base flex items-center justify-center disabled:opacity-50 disabled:cursor-not-allowed hover:bg-ctp-mauve/80 transition-colors active:scale-95"
        >
          <Send size={18} />
        </button>
      </div>
    </form>
  );
}
