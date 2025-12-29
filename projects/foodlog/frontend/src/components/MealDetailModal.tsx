import { useEffect, useState } from "react";
import { format, parseISO } from "date-fns";
import { it } from "date-fns/locale";
import { X, Trash2 } from "lucide-react";
import type { MealResponse } from "../services/api";
import { deleteFood } from "../services/api";

interface MealDetailModalProps {
  date: string; // YYYY-MM-DD
  meals: MealResponse[];
  onClose: () => void;
  onFoodDeleted?: () => void;
}

export function MealDetailModal({
  date,
  meals,
  onClose,
  onFoodDeleted,
}: MealDetailModalProps) {
  const [deletingFoodId, setDeletingFoodId] = useState<number | null>(null);

  // Close modal on ESC key
  useEffect(() => {
    const handleEsc = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    window.addEventListener("keydown", handleEsc);
    return () => window.removeEventListener("keydown", handleEsc);
  }, [onClose]);

  const handleDeleteFood = async (foodId: number, foodName: string) => {
    const confirmed = window.confirm(
      `Sei sicuro di voler cancellare "${foodName}"?`,
    );

    if (!confirmed) return;

    setDeletingFoodId(foodId);
    try {
      await deleteFood(foodId);
      // Notify parent to reload data
      if (onFoodDeleted) {
        onFoodDeleted();
      }
    } catch (error) {
      console.error("Error deleting food:", error);
      alert("Errore durante la cancellazione dell'alimento");
    } finally {
      setDeletingFoodId(null);
    }
  };

  const formattedDate = format(parseISO(date), "dd MMMM yyyy", { locale: it });

  // Group meals by type and calculate totals
  const mealsByType = meals.reduce(
    (acc, meal) => {
      if (!acc[meal.mealType]) {
        acc[meal.mealType] = [];
      }
      acc[meal.mealType].push(meal);
      return acc;
    },
    {} as Record<string, MealResponse[]>,
  );

  const getMealEmoji = (mealType: string) => {
    const type = mealType.toLowerCase();
    if (type.includes("colazione")) return "🥐";
    if (type.includes("pranzo")) return "🍝";
    if (type.includes("cena")) return "🍽️";
    if (type.includes("spuntino") || type.includes("merenda")) return "🍎";
    return "🍴";
  };

  const calculateMealTotals = (meals: MealResponse[]) => {
    return meals.reduce(
      (acc, meal) => {
        meal.foods.forEach((food) => {
          acc.calories += food.calories || 0;
          acc.protein += food.protein || 0;
          acc.carbs += food.carbs || 0;
          acc.fats += food.fats || 0;
          acc.fiber += food.fiber || 0;
        });
        return acc;
      },
      { calories: 0, protein: 0, carbs: 0, fats: 0, fiber: 0 },
    );
  };

  return (
    <div
      className="fixed inset-0 bg-black/50 z-50 flex items-center justify-center p-4"
      onClick={onClose}
    >
      <div
        className="bg-ctp-base rounded-2xl max-w-2xl w-full max-h-[90vh] overflow-y-auto shadow-2xl"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div className="sticky top-0 z-10 bg-ctp-mantle border-b border-ctp-surface0 p-4 flex items-center justify-between rounded-t-2xl">
          <div>
            <h2 className="text-xl font-bold text-ctp-text capitalize">
              📅 {formattedDate}
            </h2>
            <p className="text-sm text-ctp-subtext0">
              {meals.length} {meals.length === 1 ? "pasto" : "pasti"} registrati
            </p>
          </div>
          <button
            onClick={onClose}
            className="p-2 hover:bg-ctp-surface0 rounded-full transition-colors"
          >
            <X size={24} className="text-ctp-subtext0" />
          </button>
        </div>

        {/* Content */}
        <div className="p-4 space-y-4">
          {Object.entries(mealsByType).map(([mealType, mealsOfType]) => {
            const totals = calculateMealTotals(mealsOfType);

            return (
              <div key={mealType} className="space-y-3">
                {/* Meal Type Header */}
                <div className="flex items-center gap-2">
                  <span className="text-2xl">{getMealEmoji(mealType)}</span>
                  <div>
                    <h3 className="font-semibold text-ctp-text capitalize">
                      {mealType}
                    </h3>
                    <p className="text-sm text-ctp-subtext0">
                      {Math.round(totals.calories)} kcal • P:{" "}
                      {Math.round(totals.protein)}g • C:{" "}
                      {Math.round(totals.carbs)}g • G: {Math.round(totals.fats)}
                      g
                      {totals.fiber > 0 && ` • F: ${Math.round(totals.fiber)}g`}
                    </p>
                  </div>
                </div>

                {/* Foods */}
                <div className="space-y-2 pl-10">
                  {mealsOfType.map((meal, mealIdx) =>
                    meal.foods.map((food, foodIdx) => (
                      <div
                        key={`${mealIdx}-${foodIdx}`}
                        className="bg-ctp-surface0 border border-ctp-surface1 rounded-lg p-3 relative"
                      >
                        {/* Delete button */}
                        {food.foodId && (
                          <button
                            onClick={() =>
                              handleDeleteFood(food.foodId!, food.name)
                            }
                            disabled={deletingFoodId === food.foodId}
                            className="absolute top-2 right-2 p-1.5 bg-ctp-red text-ctp-base rounded-full hover:bg-ctp-red/80 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                            title="Cancella alimento"
                          >
                            {deletingFoodId === food.foodId ? (
                              <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin" />
                            ) : (
                              <Trash2 size={16} />
                            )}
                          </button>
                        )}

                        <div className="font-medium text-ctp-text pr-8">
                          {food.name}
                        </div>
                        <div className="text-xs text-ctp-subtext0 mt-1 flex flex-wrap gap-2">
                          {food.calories && (
                            <span className="bg-ctp-mantle px-2 py-0.5 rounded">
                              {Math.round(food.calories)} kcal
                            </span>
                          )}
                          {food.protein && (
                            <span className="bg-ctp-mantle px-2 py-0.5 rounded">
                              P: {Math.round(food.protein)}g
                            </span>
                          )}
                          {food.carbs && (
                            <span className="bg-ctp-mantle px-2 py-0.5 rounded">
                              C: {Math.round(food.carbs)}g
                            </span>
                          )}
                          {food.fats && (
                            <span className="bg-ctp-mantle px-2 py-0.5 rounded">
                              G: {Math.round(food.fats)}g
                            </span>
                          )}
                          {food.fiber && (
                            <span className="bg-ctp-mantle px-2 py-0.5 rounded">
                              F: {Math.round(food.fiber)}g
                            </span>
                          )}
                        </div>
                        {food.notes && (
                          <div className="text-xs text-ctp-subtext1 mt-1 italic">
                            {food.notes}
                          </div>
                        )}
                      </div>
                    )),
                  )}
                </div>
              </div>
            );
          })}

          {meals.length === 0 && (
            <div className="text-center py-8 text-ctp-overlay0">
              Nessun pasto registrato per questo giorno
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="sticky bottom-0 bg-ctp-mantle border-t border-ctp-surface0 p-4 rounded-b-2xl">
          <button
            onClick={onClose}
            className="w-full bg-ctp-surface2 text-ctp-text font-medium py-3 rounded-lg hover:bg-ctp-overlay0 transition-colors"
          >
            Chiudi
          </button>
        </div>
      </div>
    </div>
  );
}
