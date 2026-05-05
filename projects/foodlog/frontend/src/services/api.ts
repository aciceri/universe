export interface ChatMessage {
  role: "user" | "assistant";
  content: string;
  timestamp: Date;
  image?: string; // base64 data URL for user image messages
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

/** SSE events received from the backend */
export type ChatSSEEvent =
  | { type: "request_id"; requestId: string }
  | { type: "text"; content: string }
  | { type: "confirm"; mealData: MealSummary }
  | { type: "done"; sessionId: string }
  | { type: "error"; message: string };

const API_BASE_URL = import.meta.env.VITE_API_URL || "/api";

/**
 * Convert a backend-relative path like "/api/images/foo.jpg"
 * to an absolute URL using the configured API base.
 * In production (same origin), returns the path as-is.
 * In dev (cross-origin), prepends the backend origin.
 */
function toAbsoluteImageUrl(path: string): string {
  if (!path.startsWith("/api/")) return path;
  // API_BASE_URL is like "http://localhost:8080/api" or "/api"
  const baseOrigin = API_BASE_URL.replace(/\/api$/, "");
  if (!baseOrigin || baseOrigin === "") return path;
  return baseOrigin + path;
}

/**
 * Send a chat message and receive SSE events.
 * The onEvent callback is called for each event from the server.
 * Returns the request ID for use with confirmMeal.
 */
export async function sendChatMessage(options: {
  message: string;
  sessionId?: string;
  image?: { base64: string; mediaType: string };
  onEvent: (event: ChatSSEEvent) => void;
  signal?: AbortSignal;
}): Promise<void> {
  const { message, sessionId, image, onEvent, signal } = options;

  const body: Record<string, unknown> = { message };
  if (sessionId) body.session_id = sessionId;
  if (image) {
    body.image = {
      base64: image.base64,
      media_type: image.mediaType,
    };
  }

  const response = await fetch(`${API_BASE_URL}/chat`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
    signal,
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  if (!response.body) {
    throw new Error("No response body for SSE stream");
  }

  // Parse SSE stream
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value, { stream: true });

    // Process complete SSE messages (separated by double newlines)
    const parts = buffer.split("\n\n");
    buffer = parts.pop() || "";

    for (const part of parts) {
      if (!part.trim()) continue;

      // Parse SSE fields
      let eventType = "";
      let eventData = "";

      for (const line of part.split("\n")) {
        if (line.startsWith("event: ")) {
          eventType = line.slice(7);
        } else if (line.startsWith("data: ")) {
          eventData = line.slice(6);
        }
      }

      if (!eventData) continue;

      try {
        const parsed = JSON.parse(eventData);

        switch (eventType) {
          case "request_id":
            onEvent({ type: "request_id", requestId: parsed.requestId });
            break;
          case "text":
            onEvent({ type: "text", content: parsed.content });
            break;
          case "confirm":
            onEvent({
              type: "confirm",
              mealData: parseMealData(parsed.mealData),
            });
            break;
          case "done":
            onEvent({ type: "done", sessionId: parsed.sessionId });
            break;
          case "error":
            onEvent({ type: "error", message: parsed.message });
            break;
        }
      } catch {
        console.warn("Failed to parse SSE event:", eventData);
      }
    }
  }
}

/**
 * Parse raw meal data from the agent's save_meal tool call into our MealSummary format.
 */
function parseMealData(raw: Record<string, unknown>): MealSummary {
  const foods = ((raw.foods as Array<Record<string, unknown>>) || []).map(
    (food) => ({
      name: food.name as string,
      calories: food.calories as number | undefined,
      protein: food.protein as number | undefined,
      carbs: food.carbs as number | undefined,
      fats: food.fats as number | undefined,
      fiber: food.fiber as number | undefined,
      sugars: food.sugars as number | undefined,
      notes: food.notes as string | undefined,
    }),
  );

  const totalCalories = foods.reduce(
    (sum, food) => sum + (food.calories || 0),
    0,
  );

  return {
    meal_type: raw.meal_type as string,
    date: raw.date as string | undefined,
    total_calories: Math.round(totalCalories),
    foods,
  };
}

/**
 * Confirm or reject a pending meal.
 */
export async function confirmMeal(
  requestId: string,
  confirm: boolean,
): Promise<void> {
  const response = await fetch(`${API_BASE_URL}/chat/confirm`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ request_id: requestId, confirm }),
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }
}

/**
 * Load historical messages from a session.
 * Returns chat messages for restoring the conversation on page reload.
 */
export async function getSessionMessages(
  sessionId: string,
): Promise<ChatMessage[]> {
  const response = await fetch(`${API_BASE_URL}/session/${sessionId}/messages`);

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  const data = await response.json();
  return (data.messages || []).map(
    (msg: { role: "user" | "assistant"; content: string; image?: string }) => ({
      role: msg.role,
      content: msg.content,
      timestamp: new Date(), // SDK doesn't provide timestamps
      image: msg.image ? toAbsoluteImageUrl(msg.image) : undefined,
    }),
  );
}

/**
 * Get meals for a date range (direct SQLite query, no agent).
 */
export async function getMeals(
  fromDate: string,
  toDate: string,
): Promise<StatsResponse> {
  const response = await fetch(
    `${API_BASE_URL}/meals?from=${fromDate}&to=${toDate}`,
  );

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  return response.json();
}

export async function getMealsByDate(date: string): Promise<MealResponse[]> {
  const response = await getMeals(date, date);
  return response.meals;
}

/**
 * Delete a food item (direct SQLite operation, no agent).
 */
export async function deleteFood(foodId: number): Promise<void> {
  const response = await fetch(`${API_BASE_URL}/foods/${foodId}`, {
    method: "DELETE",
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }
}

/** Fetch the canonical session ID from the server. */
export async function getCurrentSession(): Promise<string> {
  const response = await fetch(`${API_BASE_URL}/session/current`);
  if (!response.ok) throw new Error(`API error: ${response.statusText}`);
  const data = (await response.json()) as { sessionId: string };
  return data.sessionId;
}

/** Ask the server to create a new session and return its ID. */
export async function resetSession(): Promise<string> {
  const response = await fetch(`${API_BASE_URL}/session/reset`, {
    method: "POST",
  });
  if (!response.ok) throw new Error(`API error: ${response.statusText}`);
  const data = (await response.json()) as { sessionId: string };
  return data.sessionId;
}
