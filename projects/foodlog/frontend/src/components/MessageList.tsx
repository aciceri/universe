import { useEffect, useRef } from "react";
import { format, parseISO } from "date-fns";
import { it } from "date-fns/locale";
import type { ChatMessage, MealSummary } from "../services/api";
import type { RejectedMeal } from "./ChatContainer";
import { Message } from "./Message";
import { Loader2, Check, CheckCircle, X } from "lucide-react";

interface MessageListProps {
  messages: ChatMessage[];
  isLoading: boolean;
  pendingConfirmation: MealSummary | null;
  confirmedMeal: MealSummary | null;
  rejectedMeals: RejectedMeal[];
  onConfirm: () => void;
}

export function MessageList({
  messages,
  isLoading,
  pendingConfirmation,
  confirmedMeal,
  rejectedMeals,
  onConfirm,
}: MessageListProps) {
  const bottomRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages, isLoading, pendingConfirmation, confirmedMeal, rejectedMeals]);

  // Combina messaggi e pasti rifiutati, ordinati per timestamp
  type TimelineItem =
    | { type: "message"; data: ChatMessage; timestamp: Date }
    | { type: "rejected"; data: RejectedMeal; timestamp: Date };

  const timeline: TimelineItem[] = [
    ...messages.map((msg) => ({
      type: "message" as const,
      data: msg,
      timestamp: msg.timestamp,
    })),
    ...rejectedMeals.map((rejected) => ({
      type: "rejected" as const,
      data: rejected,
      timestamp: rejected.rejectedAt,
    })),
  ].sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());

  return (
    <div className="flex-1 overflow-y-auto">
      {messages.length === 0 && (
        <div className="flex items-center justify-center h-full text-ctp-subtext0">
          <p className="text-center px-4">Dimmi cosa hai mangiato! 🍕</p>
        </div>
      )}

      {timeline.map((item, idx) => {
        if (item.type === "message") {
          return <Message key={`msg-${idx}`} message={item.data} />;
        } else {
          // Rendered rejected meal card
          const meal = item.data.meal;
          return (
            <div key={`rejected-${idx}`} className="p-4">
              <div className="bg-ctp-surface0 border-2 border-ctp-surface1 rounded-xl p-4 space-y-3 opacity-60">
                <div className="flex items-center justify-between">
                  <h3 className="font-semibold text-ctp-subtext1 capitalize flex items-center gap-2">
                    <X size={20} className="text-ctp-subtext0" />
                    {meal.meal_type}
                    {meal.date && (
                      <span className="text-xs font-normal text-ctp-subtext0">
                        ({format(parseISO(meal.date), "dd MMM", { locale: it })}
                        )
                      </span>
                    )}{" "}
                    - {meal.total_calories} kcal
                  </h3>
                  <span className="bg-ctp-surface2 text-ctp-text text-xs font-bold px-3 py-1 rounded-full">
                    ✗ Non accettato
                  </span>
                </div>

                <div className="space-y-2">
                  {meal.foods.map((food, foodIdx) => (
                    <div
                      key={foodIdx}
                      className="bg-ctp-mantle border border-ctp-surface0 rounded-lg p-3 text-sm"
                    >
                      <div className="font-medium text-ctp-subtext1">
                        {food.name}
                      </div>
                      <div className="text-xs text-ctp-subtext0 mt-1 flex gap-3">
                        {food.calories && <span>{food.calories} kcal</span>}
                        {food.protein && <span>P: {food.protein}g</span>}
                        {food.carbs && <span>C: {food.carbs}g</span>}
                        {food.fats && <span>G: {food.fats}g</span>}
                        {food.fiber && <span>Fibre: {food.fiber}g</span>}
                      </div>
                      {food.notes && (
                        <div className="text-xs text-ctp-overlay0 mt-1 italic">
                          {food.notes}
                        </div>
                      )}
                    </div>
                  ))}
                </div>
              </div>
            </div>
          );
        }
      })}

      {/* Card pasto confermato (verde) */}
      {confirmedMeal && (
        <div className="p-4">
          <div className="bg-ctp-green/10 border-2 border-ctp-green rounded-xl p-4 space-y-3">
            <div className="flex items-center justify-between">
              <h3 className="font-semibold text-ctp-text capitalize flex items-center gap-2">
                <CheckCircle size={20} className="text-ctp-green" />
                {confirmedMeal.meal_type}
                {confirmedMeal.date && (
                  <span className="text-xs font-normal text-ctp-subtext1">
                    (
                    {format(parseISO(confirmedMeal.date), "dd MMM", {
                      locale: it,
                    })}
                    )
                  </span>
                )}{" "}
                - {confirmedMeal.total_calories} kcal
              </h3>
              <span className="bg-ctp-green text-ctp-base text-xs font-bold px-3 py-1 rounded-full">
                ✓ Salvato
              </span>
            </div>

            <div className="space-y-2">
              {confirmedMeal.foods.map((food, idx) => (
                <div
                  key={idx}
                  className="bg-ctp-mantle border border-ctp-green/30 rounded-lg p-3 text-sm"
                >
                  <div className="font-medium text-ctp-text">{food.name}</div>
                  <div className="text-xs text-ctp-subtext1 mt-1 flex gap-3">
                    {food.calories && <span>{food.calories} kcal</span>}
                    {food.protein && <span>P: {food.protein}g</span>}
                    {food.carbs && <span>C: {food.carbs}g</span>}
                    {food.fats && <span>G: {food.fats}g</span>}
                    {food.fiber && <span>Fibre: {food.fiber}g</span>}
                  </div>
                  {food.notes && (
                    <div className="text-xs text-ctp-subtext0 mt-1 italic">
                      {food.notes}
                    </div>
                  )}
                </div>
              ))}
            </div>
          </div>
        </div>
      )}

      {/* Card conferma pending (gialla/primaria) */}
      {pendingConfirmation && (
        <div className="p-4">
          <div className="bg-ctp-mauve/10 border border-ctp-mauve/30 rounded-xl p-4 space-y-3">
            <div className="flex items-center justify-between">
              <h3 className="font-semibold text-ctp-text capitalize">
                {pendingConfirmation.meal_type}
                {pendingConfirmation.date && (
                  <span className="text-xs font-normal text-ctp-subtext1">
                    {" "}
                    (
                    {format(parseISO(pendingConfirmation.date), "dd MMM", {
                      locale: it,
                    })}
                    )
                  </span>
                )}{" "}
                - {pendingConfirmation.total_calories} kcal
              </h3>
            </div>

            <div className="space-y-2">
              {pendingConfirmation.foods.map((food, idx) => (
                <div key={idx} className="bg-ctp-mantle rounded-lg p-3 text-sm">
                  <div className="font-medium text-ctp-text">{food.name}</div>
                  <div className="text-xs text-ctp-subtext1 mt-1 flex gap-3">
                    {food.calories && <span>{food.calories} kcal</span>}
                    {food.protein && <span>P: {food.protein}g</span>}
                    {food.carbs && <span>C: {food.carbs}g</span>}
                    {food.fats && <span>G: {food.fats}g</span>}
                    {food.fiber && <span>Fibre: {food.fiber}g</span>}
                  </div>
                  {food.notes && (
                    <div className="text-xs text-ctp-subtext0 mt-1 italic">
                      {food.notes}
                    </div>
                  )}
                </div>
              ))}
            </div>

            <button
              onClick={onConfirm}
              disabled={isLoading}
              className="w-full bg-ctp-mauve text-ctp-base font-medium py-3 px-4 rounded-lg flex items-center justify-center gap-2 hover:bg-ctp-mauve/80 transition-colors active:scale-95 disabled:opacity-50"
            >
              <Check size={18} />
              Conferma e Salva
            </button>
          </div>
        </div>
      )}

      {isLoading && (
        <div className="flex gap-3 p-4">
          <div className="flex-shrink-0 w-8 h-8 rounded-full bg-ctp-surface1 flex items-center justify-center">
            <Loader2 size={18} className="animate-spin text-ctp-subtext0" />
          </div>
          <div className="bg-ctp-surface0 rounded-2xl px-4 py-2">
            <p className="text-sm text-ctp-subtext0">Sto pensando...</p>
          </div>
        </div>
      )}

      <div ref={bottomRef} />
    </div>
  );
}
