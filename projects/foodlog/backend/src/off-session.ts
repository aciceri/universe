/**
 * Open Food Facts session manager.
 *
 * Authenticated users bypass rate limits and get priority access during
 * peak times. We log in once, cache the session cookie, and re-authenticate
 * automatically on 401/403 or explicit expiry.
 *
 * Credentials are read from OFF_USERNAME / OFF_PASSWORD env vars.
 * If either is missing the module operates in anonymous mode.
 */

const OFF_BASE = "https://world.openfoodfacts.org";
const LOGIN_URL = `${OFF_BASE}/cgi/session.pl`;
const USER_AGENT = "FoodLog/1.0 (personal nutrition tracking app)";

let cachedCookie: string | null = null;
let loginInFlight: Promise<string | null> | null = null;

/**
 * Attempt to log in and return the session cookie value, or null if
 * credentials are missing / login fails.
 */
async function login(): Promise<string | null> {
  const username = process.env.OFF_USERNAME;
  const password = process.env.OFF_PASSWORD;

  if (!username || !password) return null;

  const body = new URLSearchParams({ user_id: username, password });

  try {
    const res = await fetch(LOGIN_URL, {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
        "User-Agent": USER_AGENT,
      },
      body: body.toString(),
      signal: AbortSignal.timeout(8000),
      redirect: "manual", // don't follow — we only need the Set-Cookie header
    });

    // OFF returns 200 or a redirect on success; 403 on bad credentials.
    const setCookie = res.headers.get("set-cookie");
    if (!setCookie) {
      console.warn("[OFF] Login response had no Set-Cookie — credentials wrong or server issue");
      return null;
    }

    // Extract just the cookie value (everything before the first ';')
    const cookie = setCookie.split(";")[0].trim();
    console.info("[OFF] Session obtained");
    return cookie;
  } catch (err) {
    console.warn("[OFF] Login failed:", err);
    return null;
  }
}

/**
 * Returns the Authorization headers for an OFF request.
 * Logs in lazily on first call; subsequent calls reuse the cached cookie.
 * Concurrent callers share the same in-flight login promise.
 */
export async function getOffHeaders(): Promise<Record<string, string>> {
  const base = { "User-Agent": USER_AGENT };

  if (!cachedCookie) {
    // Deduplicate concurrent login attempts
    if (!loginInFlight) {
      loginInFlight = login().finally(() => {
        loginInFlight = null;
      });
    }
    cachedCookie = await loginInFlight;
  }

  if (!cachedCookie) return base;
  return { ...base, Cookie: cachedCookie };
}

/**
 * Call this when a request returns 401/403 to force re-authentication
 * on the next call to getOffHeaders().
 */
export function invalidateOffSession(): void {
  cachedCookie = null;
}
