import { Database } from "bun:sqlite";

const DATABASE_PATH = process.env.DATABASE_PATH || "./foodlog.db";

let db: Database;

export function getDb(): Database {
  if (!db) {
    db = new Database(DATABASE_PATH, { create: true });
    db.exec("PRAGMA journal_mode = WAL");
    db.exec("PRAGMA foreign_keys = ON");

    // Create tables if they don't exist
    db.exec(`
      CREATE TABLE IF NOT EXISTS meal (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        date TEXT NOT NULL,
        meal_type TEXT NOT NULL,
        timestamp TEXT NOT NULL DEFAULT (datetime('now')),
        UNIQUE(date, meal_type)
      );

      CREATE TABLE IF NOT EXISTS food (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        meal_id INTEGER NOT NULL REFERENCES meal(id) ON DELETE CASCADE,
        name TEXT NOT NULL,
        calories REAL,
        protein REAL,
        carbs REAL,
        fats REAL,
        fiber REAL,
        sugars REAL,
        notes TEXT,
        timestamp TEXT NOT NULL DEFAULT (datetime('now'))
      );
    `);

    // Track uploaded images per session for restoring chat with photos
    db.exec(`
      CREATE TABLE IF NOT EXISTS chat_image (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        session_id TEXT NOT NULL,
        user_message_index INTEGER NOT NULL,
        filename TEXT NOT NULL,
        timestamp TEXT NOT NULL DEFAULT (datetime('now'))
      );
      CREATE INDEX IF NOT EXISTS idx_chat_image_session
        ON chat_image(session_id);
    `);

    // Store conversation history for OpenRouter sessions
    db.exec(`
      CREATE TABLE IF NOT EXISTS session_message (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        session_id TEXT NOT NULL,
        content_json TEXT NOT NULL,
        timestamp TEXT NOT NULL DEFAULT (datetime('now'))
      );
      CREATE INDEX IF NOT EXISTS idx_session_message_session
        ON session_message(session_id);
    `);

    // Drop the unused 'message' table if it exists from the old backend
    db.exec("DROP TABLE IF EXISTS message;");

    db.exec(`
      CREATE TABLE IF NOT EXISTS settings (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      );
    `);
    // Auto-generate the canonical session ID on first run.
    const hasSession = db
      .prepare("SELECT 1 FROM settings WHERE key = 'current_session_id'")
      .get();
    if (!hasSession) {
      db.prepare("INSERT INTO settings (key, value) VALUES ('current_session_id', ?)").run(
        crypto.randomUUID()
      );
    }
  }
  return db;
}

export interface FoodInput {
  name: string;
  calories?: number;
  protein?: number;
  carbs?: number;
  fats?: number;
  fiber?: number;
  sugars?: number;
  notes?: string;
}

export interface MealRow {
  id: number;
  date: string;
  meal_type: string;
  timestamp: string;
}

export interface FoodRow {
  id: number;
  meal_id: number;
  name: string;
  calories: number | null;
  protein: number | null;
  carbs: number | null;
  fats: number | null;
  fiber: number | null;
  sugars: number | null;
  notes: string | null;
  timestamp: string;
}

/**
 * Save a meal with its foods. If a meal with the same date+meal_type exists,
 * append foods to it (upsert behavior matching the old Haskell backend).
 */
export function saveMealWithFoods(
  mealType: string,
  foods: FoodInput[],
  date?: string,
): { mealId: number; foodIds: number[] } {
  const database = getDb();
  const mealDate = date || new Date().toISOString().split("T")[0];

  const transaction = database.transaction(() => {
    // Find existing meal or create new one
    const existingMeal = database
      .prepare("SELECT id FROM meal WHERE date = ? AND meal_type = ?")
      .get(mealDate, mealType) as { id: number } | null;

    let mealId: number;
    if (existingMeal) {
      mealId = existingMeal.id;
    } else {
      const result = database
        .prepare(
          "INSERT INTO meal (date, meal_type, timestamp) VALUES (?, ?, datetime('now'))",
        )
        .run(mealDate, mealType);
      mealId = Number(result.lastInsertRowid);
    }

    // Insert foods
    const insertFood = database.prepare(`
      INSERT INTO food (meal_id, name, calories, protein, carbs, fats, fiber, sugars, notes, timestamp)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
    `);

    const foodIds: number[] = [];
    for (const food of foods) {
      const result = insertFood.run(
        mealId,
        food.name,
        food.calories ?? null,
        food.protein ?? null,
        food.carbs ?? null,
        food.fats ?? null,
        food.fiber ?? null,
        food.sugars ?? null,
        food.notes ?? null,
      );
      foodIds.push(Number(result.lastInsertRowid));
    }

    return { mealId, foodIds };
  });

  return transaction();
}

/**
 * Save a chat image reference for session restore.
 */
export function saveChatImage(
  sessionId: string,
  userMessageIndex: number,
  filename: string,
): void {
  const database = getDb();
  database
    .prepare(
      "INSERT INTO chat_image (session_id, user_message_index, filename) VALUES (?, ?, ?)",
    )
    .run(sessionId, userMessageIndex, filename);
}

/**
 * Get all chat images for a session, ordered by user message index.
 */
export function getChatImages(
  sessionId: string,
): Array<{ userMessageIndex: number; filename: string }> {
  const database = getDb();
  return database
    .prepare(
      "SELECT user_message_index, filename FROM chat_image WHERE session_id = ? ORDER BY user_message_index",
    )
    .all(sessionId) as Array<{
    userMessageIndex: number;
    filename: string;
  }>;
}

/**
 * Get meals with their foods for a date range.
 * Matches the old backend's response format.
 */
export function getMeals(
  from: string,
  to: string,
): Array<{
  date: string;
  mealType: string;
  foods: Array<{
    foodId: number;
    name: string;
    calories: number | null;
    protein: number | null;
    carbs: number | null;
    fats: number | null;
    fiber: number | null;
    sugars: number | null;
    notes: string | null;
  }>;
}> {
  const database = getDb();

  const meals = database
    .prepare(
      "SELECT id, date, meal_type FROM meal WHERE date >= ? AND date <= ? ORDER BY date, meal_type",
    )
    .all(from, to) as MealRow[];

  if (meals.length === 0) return [];

  const mealIds = meals.map((m) => m.id);
  const placeholders = mealIds.map(() => "?").join(",");
  const foods = database
    .prepare(
      `SELECT id, meal_id, name, calories, protein, carbs, fats, fiber, sugars, notes
       FROM food WHERE meal_id IN (${placeholders}) ORDER BY id`,
    )
    .all(...mealIds) as FoodRow[];

  // Group foods by meal_id
  const foodsByMeal = new Map<number, FoodRow[]>();
  for (const food of foods) {
    const existing = foodsByMeal.get(food.meal_id) || [];
    existing.push(food);
    foodsByMeal.set(food.meal_id, existing);
  }

  return meals.map((meal) => ({
    date: meal.date,
    mealType: meal.meal_type,
    foods: (foodsByMeal.get(meal.id) || []).map((food) => ({
      foodId: food.id,
      name: food.name,
      calories: food.calories,
      protein: food.protein,
      carbs: food.carbs,
      fats: food.fats,
      fiber: food.fiber,
      sugars: food.sugars,
      notes: food.notes,
    })),
  }));
}

/**
 * Get a daily summary (total calories and macros for a given date).
 */
export function getDailySummary(date?: string): {
  date: string;
  totalCalories: number;
  totalProtein: number;
  totalCarbs: number;
  totalFats: number;
  totalFiber: number;
  totalSugars: number;
  mealCount: number;
  foodCount: number;
} {
  const database = getDb();
  const targetDate = date || new Date().toISOString().split("T")[0];

  const result = database
    .prepare(
      `SELECT
        COALESCE(SUM(f.calories), 0) as totalCalories,
        COALESCE(SUM(f.protein), 0) as totalProtein,
        COALESCE(SUM(f.carbs), 0) as totalCarbs,
        COALESCE(SUM(f.fats), 0) as totalFats,
        COALESCE(SUM(f.fiber), 0) as totalFiber,
        COALESCE(SUM(f.sugars), 0) as totalSugars,
        COUNT(DISTINCT m.id) as mealCount,
        COUNT(f.id) as foodCount
       FROM meal m
       LEFT JOIN food f ON f.meal_id = m.id
       WHERE m.date = ?`,
    )
    .get(targetDate) as {
    totalCalories: number;
    totalProtein: number;
    totalCarbs: number;
    totalFats: number;
    totalFiber: number;
    totalSugars: number;
    mealCount: number;
    foodCount: number;
  };

  return { date: targetDate, ...result };
}

/**
 * Delete a food item. If the parent meal becomes empty, delete it too.
 */
export function deleteFood(foodId: number): boolean {
  const database = getDb();

  const transaction = database.transaction(() => {
    const food = database
      .prepare("SELECT meal_id FROM food WHERE id = ?")
      .get(foodId) as { meal_id: number } | null;

    if (!food) return false;

    database.prepare("DELETE FROM food WHERE id = ?").run(foodId);

    // Check if meal is now empty
    const remaining = database
      .prepare("SELECT COUNT(*) as count FROM food WHERE meal_id = ?")
      .get(food.meal_id) as { count: number };

    if (remaining.count === 0) {
      database.prepare("DELETE FROM meal WHERE id = ?").run(food.meal_id);
    }

    return true;
  });

  return transaction();
}


// ---------------------------------------------------------------------------
// Session message storage (used by the OpenRouter agent loop)
// Messages are stored as opaque JSON matching the OpenAI message format.
// Images are stored as { type: "file_ref", filename } inside content arrays
// so we avoid persisting large base64 blobs in the DB.
// ---------------------------------------------------------------------------

export type StoredContentPart =
  | { type: "text"; text: string }
  | { type: "image_url"; image_url: { url: string } }
  | { type: "file_ref"; filename: string };

/** Opaque message shape compatible with OpenAI ChatCompletionMessageParam. */
export interface StoredMessage {
  role: "user" | "assistant" | "tool";
  /** Serialised content — may be a string or StoredContentPart[]. */
  content: string | StoredContentPart[] | null;
  /** Present on assistant messages that issued tool calls. */
  tool_calls?: Array<{
    id: string;
    type: "function";
    function: { name: string; arguments: string };
  }>;
  /** Present on tool-result messages. */
  tool_call_id?: string;
}

export function appendSessionMessage(sessionId: string, message: StoredMessage): void {
  getDb()
    .prepare(
      "INSERT INTO session_message (session_id, content_json) VALUES (?, ?)"
    )
    .run(sessionId, JSON.stringify(message));
}

export function getSessionStoredMessages(sessionId: string): StoredMessage[] {
  const rows = getDb()
    .prepare(
      "SELECT content_json FROM session_message WHERE session_id = ? ORDER BY id"
    )
    .all(sessionId) as Array<{ content_json: string }>;
  return rows.map((r) => JSON.parse(r.content_json) as StoredMessage);
}

export function sessionExists(sessionId: string): boolean {
  const row = getDb()
    .prepare(
      "SELECT 1 FROM session_message WHERE session_id = ? LIMIT 1"
    )
    .get(sessionId);
  return row !== null;
}

export function getCurrentSessionId(): string {
  const row = getDb()
    .prepare("SELECT value FROM settings WHERE key = 'current_session_id'")
    .get() as { value: string };
  return row.value;
}

export function resetCurrentSession(): string {
  const newId = crypto.randomUUID();
  getDb()
    .prepare("INSERT OR REPLACE INTO settings (key, value) VALUES ('current_session_id', ?)")
    .run(newId);
  return newId;
}