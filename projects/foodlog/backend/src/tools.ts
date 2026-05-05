import type OpenAI from "openai";
import { getMeals, getDailySummary, deleteFood } from "./db.js";
import { getOffHeaders, invalidateOffSession } from "./off-session.js";
// ---------------------------------------------------------------------------
// Tool definitions in OpenAI function-calling format
// ---------------------------------------------------------------------------

export const TOOL_DEFINITIONS: OpenAI.ChatCompletionTool[] = [
  {
    type: "function",
    function: {
      name: "save_meal",
      description:
        "Save a meal with its food items to the database. Requires user confirmation before execution.",
      parameters: {
        type: "object",
        properties: {
          meal_type: {
            type: "string",
            description: "Type of meal: colazione, pranzo, cena, spuntino, merenda, etc.",
          },
          date: {
            type: "string",
            description: "Date of the meal in YYYY-MM-DD format. Omit to use today's date.",
          },
          foods: {
            type: "array",
            description: "List of food items in this meal",
            items: {
              type: "object",
              properties: {
                name: { type: "string", description: "Name of the food item" },
                calories: { type: "number", description: "Calories in kcal" },
                protein: { type: "number", description: "Protein in grams" },
                carbs: { type: "number", description: "Carbohydrates in grams" },
                fats: { type: "number", description: "Fats in grams" },
                fiber: { type: "number", description: "Fiber in grams" },
                sugars: { type: "number", description: "Sugars in grams" },
                notes: { type: "string", description: "Notes about portion size or preparation" },
              },
              required: ["name"],
            },
          },
        },
        required: ["meal_type", "foods"],
      },
    },
  },

  {
    type: "function",
    function: {
      name: "get_meals",
      description: "Get meals and their food items for a date range",
      parameters: {
        type: "object",
        properties: {
          from: { type: "string", description: "Start date in YYYY-MM-DD format" },
          to: { type: "string", description: "End date in YYYY-MM-DD format" },
        },
        required: ["from", "to"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "get_daily_summary",
      description: "Get a summary of calories and macros for a specific day",
      parameters: {
        type: "object",
        properties: {
          date: {
            type: "string",
            description: "Date in YYYY-MM-DD format. Omit for today.",
          },
        },
      },
    },
  },
  {
    type: "function",
    function: {
      name: "delete_food",
      description: "Delete a specific food item by its ID",
      parameters: {
        type: "object",
        properties: {
          food_id: { type: "number", description: "The ID of the food item to delete" },
        },
        required: ["food_id"],
      },
    },
  },
  {
    type: "function",
    function: {
      name: "search_food",
      description:
        "Search Open Food Facts for nutritional data of a food item. Returns per-100g values. Always call this before estimating nutrition from scratch.",
      parameters: {
        type: "object",
        properties: {
          query: {
            type: "string",
            description: "Food name in Italian or English (e.g. 'pasta spaghetti', 'petto di pollo', 'mozzarella')",
          },
        },
        required: ["query"],
      },
    },
  },
];
// ---------------------------------------------------------------------------
// Tool execution — all tools except save_meal (handled separately in agent)
// ---------------------------------------------------------------------------

export async function executeTool(
  name: string,
  args: Record<string, unknown>
): Promise<unknown> {
  switch (name) {
    case "get_meals":
      return getMeals(args.from as string, args.to as string);

    case "get_daily_summary":
      return getDailySummary(args.date as string | undefined);

    case "delete_food": {
      const deleted = deleteFood(args.food_id as number);
      return {
        success: deleted,
        message: deleted ? "Alimento cancellato con successo" : "Alimento non trovato",
      };
    }

    case "search_food":
      return searchFood(args.query as string);

    default:
      return { error: `Unknown tool: ${name}` };
  }
}



async function searchFood(query: string): Promise<unknown> {
  // Use the legacy CGI endpoint — runs on the main website (100% uptime),
  // unlike /api/v2/search which uses a separate unstable microservice.
  const url = new URL("https://world.openfoodfacts.org/cgi/search.pl");
  url.searchParams.set("search_terms", query);
  url.searchParams.set("action", "process");
  url.searchParams.set("json", "1");
  url.searchParams.set("fields", "product_name,nutriments,serving_size,serving_quantity");
  url.searchParams.set("page_size", "5");
  url.searchParams.set("search_simple", "1");

  type OFFProduct = {
    product_name?: string;
    serving_size?: string;
    serving_quantity?: number;
    nutriments?: {
      "energy-kcal_100g"?: number;
      proteins_100g?: number;
      carbohydrates_100g?: number;
      fat_100g?: number;
      fiber_100g?: number;
      sugars_100g?: number;
    };
  };

  try {
    const doFetch = async () => {
      const headers = await getOffHeaders();
      return fetch(url.toString(), { headers, signal: AbortSignal.timeout(8000) });
    };

    let response = await doFetch();
    if (response.status === 401 || response.status === 403) {
      invalidateOffSession();
      response = await doFetch();
    }
    if (!response.ok) return { error: "Search failed", status: response.status };

    const data = (await response.json()) as { products?: OFFProduct[] };
    const results = (data.products ?? [])
      .filter((p) => p.product_name && p.nutriments?.["energy-kcal_100g"] != null)
      .slice(0, 5)
      .map((p) => ({
        name: p.product_name,
        per100g: {
          calories: p.nutriments!["energy-kcal_100g"],
          protein: p.nutriments!.proteins_100g ?? null,
          carbs: p.nutriments!.carbohydrates_100g ?? null,
          fats: p.nutriments!.fat_100g ?? null,
          fiber: p.nutriments!.fiber_100g ?? null,
          sugars: p.nutriments!.sugars_100g ?? null,
        },
        servingSize: p.serving_size ?? null,
        servingQuantity: p.serving_quantity ?? null,
      }));

    return { results, found: results.length };
  } catch (err) {
    return { error: "Search unavailable", detail: String(err) };
  }
}