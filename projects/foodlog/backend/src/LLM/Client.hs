module LLM.Client where

import Control.Exception (SomeException, try)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import LLM.Prompts
import LLM.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Call LLM without conversation context
callLLM :: Text -> Text -> Text -> Text -> IO (Either String LLMParsedResponse)
callLLM apiKey modelName userMessage currentTimestamp =
  callLLMWithContext apiKey modelName userMessage currentTimestamp []

-- | Call LLM with conversation context for multi-turn dialogues
callLLMWithContext :: Text -> Text -> Text -> Text -> [(Text, Text)] -> IO (Either String LLMParsedResponse)
callLLMWithContext apiKey modelName userMessage currentTimestamp contextMsgs =
  retryWithAttempts 3 $ callLLMOnce apiKey modelName userMessage currentTimestamp contextMsgs

-- | Retry logic for LLM calls
retryWithAttempts :: Int -> IO (Either String a) -> IO (Either String a)
retryWithAttempts maxAttempts action = go 1
  where
    go attempt
      | attempt > maxAttempts = return $ Left "Max retry attempts reached"
      | otherwise = do
          result <- action
          case result of
            Right val -> return $ Right val
            Left _err
              | attempt < maxAttempts -> go (attempt + 1)
              | otherwise -> return result

-- | Make a single LLM API call
callLLMOnce :: Text -> Text -> Text -> Text -> [(Text, Text)] -> IO (Either String LLMParsedResponse)
callLLMOnce apiKey modelName userMessage currentTimestamp contextMsgs = do
  -- Create TLS manager with timeout
  let managerSettings =
        tlsManagerSettings
          { managerResponseTimeout = responseTimeoutMicro 120000000 -- 120 seconds
          }
  manager <- newManager managerSettings

  let url = "https://openrouter.ai/api/v1/chat/completions"
      -- Build messages: system + context + user
      contextMessages = map (\(r, c) -> LLMMessage {role = r, content = c}) contextMsgs
      allMessages =
        [LLMMessage {role = "system", content = systemPrompt currentTimestamp}]
          ++ contextMessages
          ++ [LLMMessage {role = "user", content = userMessage}]
      reqBody =
        LLMRequest
          { model = modelName,
            messages = allMessages
          }

  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                ("Content-Type", "application/json"),
                ("HTTP-Referer", "https://foodlog.local"),
                ("X-Title", "FoodLog"),
                ("Connection", "close")
              ],
            requestBody = RequestBodyLBS $ encode reqBody
          }

  -- Execute HTTP request
  result <- try $ httpLbs request manager :: IO (Either SomeException (Response BL.ByteString))

  case result of
    Left err -> do
      return $ Left $ "HTTP request failed: " ++ show err
    Right response -> do
      let body = responseBody response
          status = statusCode $ responseStatus response

      if status /= 200
        then return $ Left $ "HTTP error " ++ show status
        else do
          -- Parse LLM response
          case decode body :: Maybe LLMResponse of
            Nothing -> return $ Left "Failed to parse LLM response"
            Just llmResp -> do
              case choices llmResp of
                [] -> return $ Left "No choices in LLM response"
                (choice : _) -> do
                  let assistantMsg = content $ message choice
                  -- Parse assistant's JSON response
                  case decode (BL.fromStrict $ TE.encodeUtf8 assistantMsg) :: Maybe LLMParsedResponse of
                    Nothing -> return $ Left $ "Failed to parse assistant message as JSON: " ++ T.unpack assistantMsg
                    Just parsed -> return $ Right parsed
