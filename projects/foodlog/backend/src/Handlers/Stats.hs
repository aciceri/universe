{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Stats where

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Database.Models
import Database.Operations
import Database.Persist
import Database.Persist.Sqlite
import GHC.Generics
import Servant

data StatsRequest = StatsRequest
  { fromDate :: Day,
    toDate :: Day
  }
  deriving (Show, Generic)

instance FromJSON StatsRequest

data FoodResponse = FoodResponse
  { foodId :: Int64,
    name :: Text,
    calories :: Maybe Double,
    protein :: Maybe Double,
    carbs :: Maybe Double,
    fats :: Maybe Double,
    fiber :: Maybe Double,
    sugars :: Maybe Double,
    notes :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON FoodResponse

data MealResponse = MealResponse
  { date :: Day,
    mealType :: Text,
    foods :: [FoodResponse]
  }
  deriving (Show, Generic)

instance ToJSON MealResponse

newtype StatsResponse = StatsResponse
  { meals :: [MealResponse]
  }
  deriving (Show, Generic)

instance ToJSON StatsResponse

-- | Handler for retrieving meal statistics within a date range
statsHandler :: Config -> Maybe Day -> Maybe Day -> Handler StatsResponse
statsHandler config mFromDate mToDate = do
  -- Validate required date parameters
  fromDate' <- case mFromDate of
    Just d -> return d
    Nothing -> throwError err400 {errBody = "Missing required query parameter: from (format: YYYY-MM-DD)"}

  toDate' <- case mToDate of
    Just d -> return d
    Nothing -> throwError err400 {errBody = "Missing required query parameter: to (format: YYYY-MM-DD)"}

  -- Fetch meals with their foods from database
  mealsWithFoods <- liftIO $ runSqlite (T.pack $ databasePath config) $ do
    getMealsInRange fromDate' toDate'

  let mealResponses = map convertMeal mealsWithFoods
  return $ StatsResponse mealResponses

convertMeal :: (Entity Meal, [Entity Food]) -> MealResponse
convertMeal (Entity _ meal, foodEntities) =
  MealResponse
    { date = mealDate meal,
      mealType = mealMealType meal,
      foods = map convertFood foodEntities
    }

convertFood :: Entity Food -> FoodResponse
convertFood (Entity fid food) =
  FoodResponse
    { foodId = fromSqlKey fid,
      name = foodName food,
      calories = foodCalories food,
      protein = foodProtein food,
      carbs = foodCarbs food,
      fats = foodFats food,
      fiber = foodFiber food,
      sugars = foodSugars food,
      notes = foodNotes food
    }
