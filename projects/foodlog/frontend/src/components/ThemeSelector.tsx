import { useState, useEffect, useRef } from "react";
import { Palette } from "lucide-react";
import { useTheme, type Theme } from "../contexts/ThemeContext";

const themes: { id: Theme; name: string; emoji: string }[] = [
  { id: "latte", name: "Latte", emoji: "🌻" },
  { id: "frappe", name: "Frappé", emoji: "🪴" },
  { id: "macchiato", name: "Macchiato", emoji: "🌺" },
  { id: "mocha", name: "Mocha", emoji: "🌿" },
];

export function ThemeSelector() {
  const { theme, setTheme } = useTheme();
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);

  // Close dropdown when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        dropdownRef.current &&
        !dropdownRef.current.contains(event.target as Node)
      ) {
        setIsOpen(false);
      }
    };

    if (isOpen) {
      document.addEventListener("mousedown", handleClickOutside);
      return () =>
        document.removeEventListener("mousedown", handleClickOutside);
    }
  }, [isOpen]);

  const handleThemeSelect = (newTheme: Theme) => {
    setTheme(newTheme);
    setIsOpen(false);
  };

  return (
    <div className="relative" ref={dropdownRef}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="p-2 rounded-lg hover:bg-ctp-base/10 transition-colors"
        title="Cambia tema"
      >
        <Palette size={20} />
      </button>

      {isOpen && (
        <div className="absolute top-full left-0 mt-2 bg-ctp-surface0 border border-ctp-surface1 rounded-lg shadow-lg overflow-hidden z-50 min-w-[160px]">
          {themes.map((t) => (
            <button
              key={t.id}
              onClick={() => handleThemeSelect(t.id)}
              className={`w-full px-4 py-2 flex items-center gap-2 hover:bg-ctp-surface1 transition-colors text-left ${
                theme === t.id ? "bg-ctp-surface1" : ""
              }`}
            >
              <span>{t.emoji}</span>
              <span className="flex-1 text-ctp-text">{t.name}</span>
              {theme === t.id && <span className="text-ctp-green">✓</span>}
            </button>
          ))}
        </div>
      )}
    </div>
  );
}
