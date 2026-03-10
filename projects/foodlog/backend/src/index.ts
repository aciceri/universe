import { Hono } from "hono";
import { cors } from "hono/cors";
import { logger } from "hono/logger";
import { api } from "./routes.js";
import { getDb } from "./db.js";

const PORT = parseInt(process.env.PORT || "8080", 10);
const CORS_ORIGINS = (
  process.env.CORS_ORIGINS || "http://localhost:5173,http://localhost:5174"
)
  .split(",")
  .map((origin) => origin.trim());

const app = new Hono();

// Middleware
app.use(
  "*",
  cors({
    origin: CORS_ORIGINS,
    allowMethods: ["GET", "POST", "DELETE", "OPTIONS"],
    allowHeaders: ["Content-Type"],
  }),
);
app.use("*", logger());

// Mount API routes
app.route("/", api);

// Health check
app.get("/health", (c) => c.json({ status: "ok" }));

// Initialize database on startup
getDb();
console.log(`Foodlog backend starting on port ${PORT}`);
console.log(`CORS origins: ${CORS_ORIGINS.join(", ")}`);

export default {
  port: PORT,
  fetch: app.fetch,
  // SSE streams can stay open for minutes (waiting for Claude + user confirmation)
  idleTimeout: 255, // ~4 minutes (max allowed by Bun)
};
