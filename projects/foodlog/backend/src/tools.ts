import { tool, createSdkMcpServer } from "@anthropic-ai/claude-agent-sdk";
import { z } from "zod";
import {
  saveMealWithFoods,
  getMeals,
  getDailySummary,
  deleteFood,
  type FoodInput,
} from "./db.js";

const foodSchema = z.object({
  name: z.string().describe("Name of the food item"),
  calories: z.number().optional().describe("Calories in kcal"),
  protein: z.number().optional().describe("Protein in grams"),
  carbs: z.number().optional().describe("Carbohydrates in grams"),
  fats: z.number().optional().describe("Fats in grams"),
  fiber: z.number().optional().describe("Fiber in grams"),
  sugars: z.number().optional().describe("Sugars in grams"),
  notes: z.string().optional().describe("Notes about portion size or preparation"),
});

const saveMealTool = tool(
  "save_meal",
  "Save a meal with its food items to the database. The user must confirm before this is executed.",
  {
    meal_type: z
      .string()
      .describe(
        "Type of meal: colazione, pranzo, cena, spuntino, merenda, etc.",
      ),
    date: z
      .string()
      .optional()
      .describe(
        "Date of the meal in YYYY-MM-DD format. Omit to use today's date.",
      ),
    foods: z.array(foodSchema).describe("List of food items in this meal"),
  },
  async (args) => {
    const foods: FoodInput[] = args.foods.map((food) => ({
      name: food.name,
      calories: food.calories,
      protein: food.protein,
      carbs: food.carbs,
      fats: food.fats,
      fiber: food.fiber,
      sugars: food.sugars,
      notes: food.notes,
    }));

    const result = saveMealWithFoods(args.meal_type, foods, args.date);

    const totalCalories = foods.reduce(
      (sum, food) => sum + (food.calories || 0),
      0,
    );

    return {
      content: [
        {
          type: "text" as const,
          text: JSON.stringify({
            success: true,
            mealId: result.mealId,
            foodIds: result.foodIds,
            totalCalories: Math.round(totalCalories),
            foodCount: foods.length,
          }),
        },
      ],
    };
  },
);

const getMealsTool = tool(
  "get_meals",
  "Get meals and their food items for a date range",
  {
    from: z.string().describe("Start date in YYYY-MM-DD format"),
    to: z.string().describe("End date in YYYY-MM-DD format"),
  },
  async (args) => {
    const meals = getMeals(args.from, args.to);
    return {
      content: [
        {
          type: "text" as const,
          text: JSON.stringify({ meals }),
        },
      ],
    };
  },
);

const getDailySummaryTool = tool(
  "get_daily_summary",
  "Get a summary of calories and macros for a specific day",
  {
    date: z
      .string()
      .optional()
      .describe("Date in YYYY-MM-DD format. Omit for today."),
  },
  async (args) => {
    const summary = getDailySummary(args.date);
    return {
      content: [
        {
          type: "text" as const,
          text: JSON.stringify(summary),
        },
      ],
    };
  },
);

const deleteFoodTool = tool(
  "delete_food",
  "Delete a specific food item by its ID",
  {
    food_id: z.number().describe("The ID of the food item to delete"),
  },
  async (args) => {
    const deleted = deleteFood(args.food_id);
    return {
      content: [
        {
          type: "text" as const,
          text: JSON.stringify({
            success: deleted,
            message: deleted
              ? "Alimento cancellato con successo"
              : "Alimento non trovato",
          }),
        },
      ],
    };
  },
);

/**
 * Create a fresh MCP server instance per request.
 * The SDK connects a transport to the server on each query() call,
 * so reusing the same instance across requests causes
 * "Already connected to a transport" errors.
 */
export function createFoodlogMcpServer() {
  return createSdkMcpServer({
    name: "foodlog",
    version: "0.2.0",
    tools: [saveMealTool, getMealsTool, getDailySummaryTool, deleteFoodTool],
  });
}

/**
 * The tool name as it will appear in canUseTool callback.
 * MCP tools follow the pattern: mcp__<server-name>__<tool-name>
 */
export const SAVE_MEAL_TOOL_NAME = "mcp__foodlog__save_meal";
