export interface ChatMessage {
  role: "user" | "assistant";
  content: string;
  timestamp: Date;
  summary?: MealSummary;
}

export interface ChatRequest {
  message: string;
  timestamp?: string;
  confirmed?: boolean;
  meal_data?: {
    meal_type: string;
    date?: string; // YYYY-MM-DD format
    foods: FoodData[];
  };
  context?: Array<{
    role: string;
    content: string;
  }>;
}

export interface ChatResponse {
  needs_confirmation: boolean;
  message: string;
  summary?: MealSummary;
  error?: string;
}

export interface MealSummary {
  meal_type: string;
  date?: string; // YYYY-MM-DD format
  total_calories?: number;
  foods: FoodData[];
}

export interface FoodData {
  foodId?: number;
  name: string;
  calories?: number;
  protein?: number;
  carbs?: number;
  fats?: number;
  fiber?: number;
  sugars?: number;
  notes?: string;
}

export interface MealResponse {
  date: string; // YYYY-MM-DD
  mealType: string;
  foods: FoodData[];
}

export interface StatsResponse {
  meals: MealResponse[];
}

const API_BASE_URL = import.meta.env.VITE_API_URL || "/api";

export async function sendMessage(
  message: string,
  confirmed?: boolean,
  mealData?: MealSummary,
  contextMessages?: ChatMessage[],
): Promise<ChatResponse> {
  const now = new Date();
  const localTimestamp = now.toLocaleString("it-IT", {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
  });

  const requestBody: ChatRequest = {
    message,
    timestamp: localTimestamp,
    confirmed,
  };

  if (confirmed && mealData) {
    requestBody.meal_data = {
      meal_type: mealData.meal_type,
      date: mealData.date,
      foods: mealData.foods,
    };
  }

  // Add conversation context for multi-turn dialogue
  if (contextMessages && contextMessages.length > 0) {
    requestBody.context = contextMessages.map((msg) => ({
      role: msg.role,
      content: msg.content,
    }));
  }

  console.log("Sending request:", JSON.stringify(requestBody, null, 2));

  const response = await fetch(`${API_BASE_URL}/chat`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(requestBody),
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  const data: ChatResponse = await response.json();
  return data;
}

export async function getMeals(
  fromDate: string,
  toDate: string,
): Promise<StatsResponse> {
  const response = await fetch(
    `${API_BASE_URL}/meals?from=${fromDate}&to=${toDate}`,
    {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
      },
    },
  );

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  const data: StatsResponse = await response.json();
  return data;
}

export async function getMealsByDate(date: string): Promise<MealResponse[]> {
  const response = await getMeals(date, date);
  return response.meals;
}

export async function deleteFood(foodId: number): Promise<void> {
  const response = await fetch(`${API_BASE_URL}/foods/${foodId}`, {
    method: "DELETE",
    headers: {
      "Content-Type": "application/json",
    },
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }
}
