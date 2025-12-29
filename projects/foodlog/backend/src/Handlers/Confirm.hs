module Handlers.Confirm where

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Database.Operations
import Database.Persist.Sqlite (runSqlite)
import Database.Utils (foodDataToInput)
import GHC.Generics
import LLM.Types
import Servant

-- | Request to confirm and save a meal
data ConfirmRequest = ConfirmRequest
  { mealType :: Text,
    mealDate :: Maybe Day, -- Added to support past dates
    foods :: [FoodData]
  }
  deriving (Show, Generic)

instance FromJSON ConfirmRequest where
  parseJSON = withObject "ConfirmRequest" $ \v ->
    ConfirmRequest
      <$> v .: "meal_type"
      <*> v .:? "date"
      <*> v .: "foods"

data ConfirmResponse = ConfirmResponse
  { success :: Bool,
    message :: Text
  }
  deriving (Show, Generic)

instance ToJSON ConfirmResponse

-- | Handler for confirming and saving a meal
confirmHandler :: Config -> ConfirmRequest -> Handler ConfirmResponse
confirmHandler config req = do
  let foodInputs = map foodDataToInput (Handlers.Confirm.foods req)
      mealDateToSave = Handlers.Confirm.mealDate req

  liftIO $ runNoLoggingT $ runSqlite (T.pack $ databasePath config) $ do
    saveMealWithFoods (Handlers.Confirm.mealType req) mealDateToSave foodInputs

  return
    ConfirmResponse
      { success = True,
        Handlers.Confirm.message = "Pasto salvato con successo! ✅"
      }
