import { useState } from "react";
import { subDays, format } from "date-fns";

export type DateRange = {
  from: string; // YYYY-MM-DD
  to: string; // YYYY-MM-DD
};

type Preset = "7d" | "30d" | "90d" | "custom";

interface DateRangeSelectorProps {
  onRangeChange: (range: DateRange) => void;
}

export function DateRangeSelector({ onRangeChange }: DateRangeSelectorProps) {
  const [selectedPreset, setSelectedPreset] = useState<Preset>("7d");
  const [customFrom, setCustomFrom] = useState("");
  const [customTo, setCustomTo] = useState("");

  const getPresetRange = (preset: Preset): DateRange => {
    const today = new Date();
    const todayStr = format(today, "yyyy-MM-dd");

    switch (preset) {
      case "7d":
        return { from: format(subDays(today, 6), "yyyy-MM-dd"), to: todayStr };
      case "30d":
        return { from: format(subDays(today, 29), "yyyy-MM-dd"), to: todayStr };
      case "90d":
        return { from: format(subDays(today, 89), "yyyy-MM-dd"), to: todayStr };
      case "custom":
        return { from: customFrom, to: customTo };
    }
  };

  const handlePresetClick = (preset: Preset) => {
    setSelectedPreset(preset);
    if (preset !== "custom") {
      const range = getPresetRange(preset);
      onRangeChange(range);
    }
  };

  const handleCustomApply = () => {
    if (customFrom && customTo) {
      onRangeChange({ from: customFrom, to: customTo });
    }
  };

  return (
    <div className="bg-ctp-surface0 p-4 space-y-3">
      <div className="flex gap-2 flex-wrap">
        <button
          onClick={() => handlePresetClick("7d")}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            selectedPreset === "7d"
              ? "bg-ctp-surface2 text-ctp-text"
              : "bg-ctp-surface1 text-ctp-text hover:bg-ctp-surface2"
          }`}
        >
          Ultimi 7 giorni
        </button>
        <button
          onClick={() => handlePresetClick("30d")}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            selectedPreset === "30d"
              ? "bg-ctp-surface2 text-ctp-text"
              : "bg-ctp-surface1 text-ctp-text hover:bg-ctp-surface2"
          }`}
        >
          Ultimi 30 giorni
        </button>
        <button
          onClick={() => handlePresetClick("90d")}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            selectedPreset === "90d"
              ? "bg-ctp-surface2 text-ctp-text"
              : "bg-ctp-surface1 text-ctp-text hover:bg-ctp-surface2"
          }`}
        >
          Ultimi 90 giorni
        </button>
        <button
          onClick={() => handlePresetClick("custom")}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            selectedPreset === "custom"
              ? "bg-ctp-surface2 text-ctp-text"
              : "bg-ctp-surface1 text-ctp-text hover:bg-ctp-surface2"
          }`}
        >
          Personalizzato
        </button>
      </div>

      {selectedPreset === "custom" && (
        <div className="flex gap-2 items-end flex-wrap">
          <div className="flex-1 min-w-[140px]">
            <label className="block text-sm font-medium text-ctp-subtext0 mb-1">
              Da
            </label>
            <input
              type="date"
              value={customFrom}
              onChange={(e) => setCustomFrom(e.target.value)}
              className="w-full px-3 py-2 bg-ctp-mantle border border-ctp-surface2 text-ctp-text rounded-lg focus:ring-2 focus:ring-ctp-mauve focus:border-transparent"
            />
          </div>
          <div className="flex-1 min-w-[140px]">
            <label className="block text-sm font-medium text-ctp-subtext0 mb-1">
              A
            </label>
            <input
              type="date"
              value={customTo}
              onChange={(e) => setCustomTo(e.target.value)}
              className="w-full px-3 py-2 bg-ctp-mantle border border-ctp-surface2 text-ctp-text rounded-lg focus:ring-2 focus:ring-ctp-mauve focus:border-transparent"
            />
          </div>
          <button
            onClick={handleCustomApply}
            disabled={!customFrom || !customTo}
            className="px-4 py-2 bg-ctp-surface2 text-ctp-text rounded-lg font-medium hover:bg-ctp-overlay0 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
          >
            Applica
          </button>
        </div>
      )}
    </div>
  );
}
