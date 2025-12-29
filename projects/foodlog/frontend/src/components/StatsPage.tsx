import { useState, useEffect } from "react";
import { format, subDays } from "date-fns";
import { it } from "date-fns/locale";
import {
  AreaChart,
  Area,
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
} from "recharts";
import { DateRangeSelector, type DateRange } from "./DateRangeSelector";
import { getMeals, getMealsByDate, type MealResponse } from "../services/api";
import { MealDetailModal } from "./MealDetailModal";

interface DailyData {
  date: string;
  displayDate: string;
  calories: number;
  protein: number;
  carbs: number;
  fats: number;
  fiber: number;
  meals: MealResponse[];
}

export function StatsPage() {
  const [dateRange, setDateRange] = useState<DateRange>({
    from: format(subDays(new Date(), 6), "yyyy-MM-dd"),
    to: format(new Date(), "yyyy-MM-dd"),
  });
  const [dailyData, setDailyData] = useState<DailyData[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [selectedDate, setSelectedDate] = useState<string | null>(null);
  const [selectedMeals, setSelectedMeals] = useState<MealResponse[]>([]);

  useEffect(() => {
    loadData();
  }, [dateRange]);

  const loadData = async () => {
    setIsLoading(true);
    try {
      const response = await getMeals(dateRange.from, dateRange.to);
      const aggregated = aggregateByDay(response.meals);
      setDailyData(aggregated);
    } catch (error) {
      console.error("Error loading stats:", error);
    } finally {
      setIsLoading(false);
    }
  };

  const aggregateByDay = (meals: MealResponse[]): DailyData[] => {
    const dayMap = new Map<string, DailyData>();

    meals.forEach((meal) => {
      const existing = dayMap.get(meal.date);
      const mealNutrients = calculateMealNutrients(meal);

      if (existing) {
        existing.calories += mealNutrients.calories;
        existing.protein += mealNutrients.protein;
        existing.carbs += mealNutrients.carbs;
        existing.fats += mealNutrients.fats;
        existing.fiber += mealNutrients.fiber;
        existing.meals.push(meal);
      } else {
        dayMap.set(meal.date, {
          date: meal.date,
          displayDate: format(new Date(meal.date), "dd MMM", { locale: it }),
          calories: mealNutrients.calories,
          protein: mealNutrients.protein,
          carbs: mealNutrients.carbs,
          fats: mealNutrients.fats,
          fiber: mealNutrients.fiber,
          meals: [meal],
        });
      }
    });

    return Array.from(dayMap.values()).sort((a, b) =>
      a.date.localeCompare(b.date),
    );
  };

  const calculateMealNutrients = (meal: MealResponse) => {
    return meal.foods.reduce(
      (acc, food) => ({
        calories: acc.calories + (food.calories || 0),
        protein: acc.protein + (food.protein || 0),
        carbs: acc.carbs + (food.carbs || 0),
        fats: acc.fats + (food.fats || 0),
        fiber: acc.fiber + (food.fiber || 0),
      }),
      { calories: 0, protein: 0, carbs: 0, fats: 0, fiber: 0 },
    );
  };

  const averageCalories =
    dailyData.length > 0
      ? Math.round(
          dailyData.reduce((sum, day) => sum + day.calories, 0) /
            dailyData.length,
        )
      : 0;

  const handleDayClick = async (date: string) => {
    setSelectedDate(date);
    try {
      const meals = await getMealsByDate(date);
      setSelectedMeals(meals);
    } catch (error) {
      console.error("Error loading meals for date:", error);
      setSelectedMeals([]);
    }
  };

  const handleCloseModal = () => {
    setSelectedDate(null);
    setSelectedMeals([]);
  };

  const handleFoodDeleted = async () => {
    // Reload meals for the selected date
    if (selectedDate) {
      try {
        const meals = await getMealsByDate(selectedDate);
        setSelectedMeals(meals);
      } catch (error) {
        console.error("Error reloading meals:", error);
      }
    }
    // Reload all stats data
    await loadData();
  };

  return (
    <div className="flex flex-col h-full bg-ctp-base">
      <div className="flex-1 overflow-y-auto">
        <DateRangeSelector onRangeChange={setDateRange} />

        {isLoading ? (
          <div className="flex items-center justify-center p-8">
            <p className="text-ctp-subtext0">Caricamento...</p>
          </div>
        ) : (
          <>
            {/* Summary Cards */}
            <div className="p-4 grid grid-cols-2 gap-3">
              <div className="bg-ctp-surface0 rounded-lg p-4 shadow-sm">
                <p className="text-sm text-ctp-subtext0">Media Giornaliera</p>
                <p className="text-2xl font-bold text-ctp-mauve">
                  {averageCalories}
                </p>
                <p className="text-xs text-ctp-subtext1">kcal/giorno</p>
              </div>
              <div className="bg-ctp-surface0 rounded-lg p-4 shadow-sm">
                <p className="text-sm text-ctp-subtext0">Giorni Tracciati</p>
                <p className="text-2xl font-bold text-ctp-mauve">
                  {dailyData.length}
                </p>
                <p className="text-xs text-ctp-subtext1">giorni</p>
              </div>
            </div>

            {/* Bar Chart - Calorie */}
            <div className="bg-ctp-surface0 m-4 p-4 rounded-lg shadow-sm">
              <h2 className="text-md font-semibold text-ctp-text mb-3">
                Calorie Giornaliere
              </h2>
              <ResponsiveContainer width="100%" height={200}>
                <BarChart data={dailyData}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
                  <XAxis
                    dataKey="displayDate"
                    tick={{ fontSize: 12 }}
                    stroke="#999"
                  />
                  <YAxis tick={{ fontSize: 12 }} stroke="#999" />
                  <Tooltip
                    contentStyle={{
                      backgroundColor: "#fff",
                      border: "1px solid #ddd",
                      borderRadius: "8px",
                    }}
                  />
                  <Bar
                    dataKey="calories"
                    fill="#22c55e"
                    radius={[8, 8, 0, 0]}
                  />
                </BarChart>
              </ResponsiveContainer>
            </div>

            {/* Stacked Area Chart - Macronutrienti */}
            <div className="bg-ctp-surface0 m-4 p-4 rounded-lg shadow-sm">
              <h2 className="text-md font-semibold text-ctp-text mb-3">
                Macronutrienti nel Tempo
              </h2>
              <ResponsiveContainer width="100%" height={200}>
                <AreaChart data={dailyData}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
                  <XAxis
                    dataKey="displayDate"
                    tick={{ fontSize: 12 }}
                    stroke="#999"
                  />
                  <YAxis tick={{ fontSize: 12 }} stroke="#999" />
                  <Tooltip
                    contentStyle={{
                      backgroundColor: "#fff",
                      border: "1px solid #ddd",
                      borderRadius: "8px",
                    }}
                  />
                  <Legend wrapperStyle={{ fontSize: "12px" }} />
                  <Area
                    type="monotone"
                    dataKey="protein"
                    stackId="1"
                    stroke="#3b82f6"
                    fill="#3b82f6"
                    name="Proteine (g)"
                  />
                  <Area
                    type="monotone"
                    dataKey="carbs"
                    stackId="1"
                    stroke="#f59e0b"
                    fill="#f59e0b"
                    name="Carboidrati (g)"
                  />
                  <Area
                    type="monotone"
                    dataKey="fats"
                    stackId="1"
                    stroke="#ef4444"
                    fill="#ef4444"
                    name="Grassi (g)"
                  />
                  <Area
                    type="monotone"
                    dataKey="fiber"
                    stackId="1"
                    stroke="#10b981"
                    fill="#10b981"
                    name="Fibre (g)"
                  />
                </AreaChart>
              </ResponsiveContainer>
            </div>

            {/* Table */}
            <div className="bg-ctp-surface0 m-4 p-4 rounded-lg shadow-sm">
              <h2 className="text-md font-semibold text-ctp-text mb-3">
                Dettaglio per Giorno
              </h2>
              <div className="overflow-x-auto">
                <table className="w-full text-sm">
                  <thead className="bg-ctp-mantle text-left">
                    <tr>
                      <th className="p-2 font-semibold text-ctp-subtext1">
                        Data
                      </th>
                      <th className="p-2 font-semibold text-ctp-subtext1">
                        Kcal
                      </th>
                      <th className="p-2 font-semibold text-ctp-subtext1">P</th>
                      <th className="p-2 font-semibold text-ctp-subtext1">C</th>
                      <th className="p-2 font-semibold text-ctp-subtext1">G</th>
                      <th className="p-2 font-semibold text-ctp-subtext1">F</th>
                      <th className="p-2 font-semibold text-ctp-subtext1">
                        Pasti
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {dailyData.map((day) => (
                      <tr
                        key={day.date}
                        onClick={() => handleDayClick(day.date)}
                        className="border-t border-ctp-surface1 cursor-pointer hover:bg-ctp-surface1 transition-colors"
                      >
                        <td className="p-2 text-ctp-text">{day.displayDate}</td>
                        <td className="p-2 font-medium text-ctp-mauve">
                          {Math.round(day.calories)}
                        </td>
                        <td className="p-2 text-ctp-subtext0">
                          {Math.round(day.protein)}g
                        </td>
                        <td className="p-2 text-ctp-subtext0">
                          {Math.round(day.carbs)}g
                        </td>
                        <td className="p-2 text-ctp-subtext0">
                          {Math.round(day.fats)}g
                        </td>
                        <td className="p-2 text-ctp-subtext0">
                          {Math.round(day.fiber)}g
                        </td>
                        <td className="p-2 text-ctp-subtext1">
                          {day.meals.length}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Modal for meal details */}
      {selectedDate && (
        <MealDetailModal
          date={selectedDate}
          meals={selectedMeals}
          onClose={handleCloseModal}
          onFoodDeleted={handleFoodDeleted}
        />
      )}
    </div>
  );
}
