{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api
import Config
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Models
import Database.Persist.Sqlite (runMigration, runSqlite)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant

main :: IO ()
main = do
  putStrLn "🍕 FoodLog Backend starting..."

  -- Load configuration from environment variables
  config <- loadConfig
  putStrLn $ "📝 Config loaded:"
  putStrLn $ "  - Model: " ++ show (llmModel config)
  putStrLn $ "  - Database: " ++ databasePath config
  putStrLn $ "  - Port: " ++ show (port config)

  -- Run database migrations
  putStrLn "🗄️  Running database migrations..."
  runNoLoggingT $ runSqlite (T.pack $ databasePath config) $ runMigration migrateAll
  putStrLn "✅ Database ready"

  -- Start HTTP server
  let portNum = port config
  putStrLn $ "🚀 Server running on http://localhost:" ++ show portNum
  putStrLn "   API endpoints:"
  putStrLn "   - POST /api/chat"
  putStrLn "   - POST /api/chat/confirm"
  putStrLn "   - GET  /api/meals?from=YYYY-MM-DD&to=YYYY-MM-DD"
  putStrLn "   - DELETE /api/foods/:id"

  let allowedOrigins = map TE.encodeUtf8 (Config.corsOrigins config) :: [ByteString]
  putStrLn $ "   CORS allowed origins: " ++ show (Config.corsOrigins config)

  run portNum $ cors (const $ Just $ mkCorsPolicy allowedOrigins) $ logStdout $ serve api (server config)
  where
    -- Create CORS policy from allowed origins
    mkCorsPolicy :: [ByteString] -> CorsResourcePolicy
    mkCorsPolicy origins =
      simpleCorsResourcePolicy
        { Network.Wai.Middleware.Cors.corsOrigins = Just (origins, True),
          corsRequestHeaders = ["Content-Type"],
          corsMethods = ["GET", "POST", "DELETE", "OPTIONS"]
        }
