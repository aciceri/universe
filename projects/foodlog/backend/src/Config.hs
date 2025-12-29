module Config where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { openRouterApiKey :: Text,
    llmModel :: Text,
    databasePath :: FilePath,
    port :: Int,
    corsOrigins :: [Text]
  }
  deriving (Show)

loadConfig :: IO Config
loadConfig = do
  apiKey <- getEnvRequired "OPENROUTER_API_KEY"
  model <- getEnvDefault "LLM_MODEL" "google/gemini-flash-1.5-8b"
  dbPath <- getEnvDefault "DATABASE_PATH" "./foodlog.db"
  portStr <- getEnvDefault "PORT" "8080"
  origins <- getEnvDefault "CORS_ORIGINS" "http://localhost:5173,http://localhost:5174"

  -- Validate port number
  portNum <- case readMaybe portStr of
    Just p | p > 0 && p <= 65535 -> return p
    _ -> error $ "Invalid PORT value: " ++ portStr ++ " (must be a number between 1 and 65535)"

  return
    Config
      { openRouterApiKey = T.pack apiKey,
        llmModel = T.pack model,
        databasePath = dbPath,
        port = portNum,
        corsOrigins = map T.strip $ T.splitOn "," $ T.pack origins
      }

getEnvRequired :: String -> IO String
getEnvRequired key = do
  mVal <- lookupEnv key
  case mVal of
    Nothing -> error $ "Missing required environment variable: " ++ key
    Just val -> return val

getEnvDefault :: String -> String -> IO String
getEnvDefault key def = do
  mVal <- lookupEnv key
  return $ maybe def id mVal
