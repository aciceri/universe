module Handlers.Chat where

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, getCurrentTime)
import Database.Operations (saveMealWithFoods)
import Database.Persist.Sqlite (runSqlite)
import Database.Utils (foodDataToInput)
import GHC.Generics
import LLM.Client
import LLM.Types (FoodData (..))
import qualified LLM.Types as LLM
import Servant

-- | API Types for chat endpoint
data ChatRequest = ChatRequest
  { message :: Text,
    timestamp :: Maybe Text,
    confirmed :: Maybe Bool,
    mealData :: Maybe MealData,
    context :: Maybe [ContextMessage]
  }
  deriving (Show, Generic)

instance FromJSON ChatRequest where
  parseJSON = withObject "ChatRequest" $ \v ->
    ChatRequest
      <$> v .: "message"
      <*> v .:? "timestamp"
      <*> v .:? "confirmed"
      <*> v .:? "meal_data"
      <*> v .:? "context"

data ContextMessage = ContextMessage
  { ctxRole :: Text,
    ctxContent :: Text
  }
  deriving (Show, Generic)

instance FromJSON ContextMessage where
  parseJSON = withObject "ContextMessage" $ \v ->
    ContextMessage
      <$> v .: "role"
      <*> v .: "content"

data MealData = MealData
  { mdMealType :: Text,
    mdMealDate :: Maybe Day,
    mdFoods :: [FoodData]
  }
  deriving (Show, Generic)

instance FromJSON MealData where
  parseJSON = withObject "MealData" $ \v ->
    MealData
      <$> v .: "meal_type"
      <*> v .:? "date"
      <*> v .: "foods"

data ChatResponse = ChatResponse
  { needsConfirmation :: Bool,
    responseMessage :: Text,
    summary :: Maybe MealSummary,
    errorMsg :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON cr =
    object $
      [ "needs_confirmation" .= needsConfirmation cr,
        "message" .= responseMessage cr
      ]
        ++ maybe [] (\s -> ["summary" .= s]) (summary cr)
        ++ maybe [] (\e -> ["error" .= e]) (errorMsg cr)

data MealSummary = MealSummary
  { msMealType :: Text,
    msMealDate :: Maybe Day,
    msTotalCalories :: Maybe Double,
    msFoods :: [FoodData]
  }
  deriving (Show, Generic)

instance ToJSON MealSummary where
  toJSON ms =
    object
      [ "meal_type" .= msMealType ms,
        "date" .= msMealDate ms,
        "total_calories" .= msTotalCalories ms,
        "foods" .= msFoods ms
      ]

chatHandler :: Config -> ChatRequest -> Handler ChatResponse
chatHandler config req = do
  -- Use timestamp from frontend, or get current time if not provided
  currentTimestamp <- case timestamp req of
    Just ts -> return ts
    Nothing -> do
      now <- liftIO getCurrentTime
      return $ T.pack $ show now

  -- Check if this is a confirmation request (user confirming meal data)
  case (confirmed req, mealData req) of
    (Just True, Just md) -> do
      -- Save confirmed meal to database
      let foodInputs = map foodDataToInput (mdFoods md)
          mealDateToSave = mdMealDate md
      liftIO $ runNoLoggingT $ runSqlite (T.pack $ databasePath config) $ do
        saveMealWithFoods (mdMealType md) mealDateToSave foodInputs

      return $
        ChatResponse
          { needsConfirmation = False,
            responseMessage = "Salvato! ✅",
            summary = Nothing,
            errorMsg = Nothing
          }
    _ -> do
      -- Process new message with LLM, including conversation context
      let contextMsgs = maybe [] (map (\cm -> (ctxRole cm, ctxContent cm))) (context req)
      result <-
        liftIO $
          callLLMWithContext
            (openRouterApiKey config)
            (llmModel config)
            (message req)
            currentTimestamp
            contextMsgs

      case result of
        Left err ->
          return $
            ChatResponse
              { needsConfirmation = False,
                responseMessage = "Mi dispiace, c'è stato un errore. Riprova!",
                summary = Nothing,
                errorMsg = Just $ T.pack err
              }
        Right parsed ->
          if LLM.needsConfirmation parsed && not (null $ LLM.foods parsed)
            then do
              -- LLM parsed foods and needs user confirmation
              let totalCal = sum $ map (maybe 0 id . foodCalories) (LLM.foods parsed)
              return $
                ChatResponse
                  { needsConfirmation = True,
                    responseMessage = LLM.responseMessage parsed,
                    summary =
                      Just $
                        MealSummary
                          { msMealType = maybe "altro" id (LLM.mealType parsed),
                            msMealDate = LLM.mealDate parsed,
                            msTotalCalories = Just totalCal,
                            msFoods = LLM.foods parsed
                          },
                    errorMsg = Nothing
                  }
            else
              -- LLM response doesn't require confirmation (e.g., clarification question)
              return $
                ChatResponse
                  { needsConfirmation = False,
                    responseMessage = LLM.responseMessage parsed,
                    summary = Nothing,
                    errorMsg = Nothing
                  }
