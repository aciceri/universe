module Database.Operations where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (Day, UTCTime, getCurrentTime, utctDay)
import Database.Models
import Database.Persist
import Database.Persist.Sqlite

-- | Save a meal with associated foods. If the meal already exists for the given date and type,
-- foods are added to the existing meal.
saveMealWithFoods :: (MonadIO m) => Text -> Maybe Day -> [FoodInput] -> ReaderT SqlBackend m ()
saveMealWithFoods mealType mDate foodInputs = do
  now <- liftIO getCurrentTime
  let mealDay = case mDate of
        Just d -> d
        Nothing -> utctDay now

  -- Get or create meal
  mealId <-
    getBy (UniqueMeal mealDay mealType) >>= \case
      Just (Entity mid _) -> return mid
      Nothing ->
        insert $
          Meal
            { mealDate = mealDay,
              mealMealType = mealType,
              mealTimestamp = now
            }

  -- Insert all foods
  mapM_ (insertFood mealId now) foodInputs

insertFood :: (MonadIO m) => MealId -> UTCTime -> FoodInput -> ReaderT SqlBackend m ()
insertFood mealId timestamp foodInput = do
  insert_ $
    Food
      { foodMealId = mealId,
        foodName = fiName foodInput,
        foodCalories = fiCalories foodInput,
        foodProtein = fiProtein foodInput,
        foodCarbs = fiCarbs foodInput,
        foodFats = fiFats foodInput,
        foodFiber = fiFiber foodInput,
        foodSugars = fiSugars foodInput,
        foodNotes = fiNotes foodInput,
        foodTimestamp = timestamp
      }

data FoodInput = FoodInput
  { fiName :: Text,
    fiCalories :: Maybe Double,
    fiProtein :: Maybe Double,
    fiCarbs :: Maybe Double,
    fiFats :: Maybe Double,
    fiFiber :: Maybe Double,
    fiSugars :: Maybe Double,
    fiNotes :: Maybe Text
  }
  deriving (Show)

-- | Get meals with foods in a date range. Uses a single query to avoid N+1 problem.
getMealsInRange :: (MonadIO m) => Day -> Day -> ReaderT SqlBackend m [(Entity Meal, [Entity Food])]
getMealsInRange fromDate toDate = do
  -- Fetch all meals in the date range
  meals <- selectList [MealDate >=. fromDate, MealDate <=. toDate] [Asc MealDate]

  if null meals
    then return []
    else do
      -- Extract meal IDs
      let mealIds = map entityKey meals

      -- Fetch all foods for these meals in a single query
      allFoods <- selectList [FoodMealId <-. mealIds] [Asc FoodMealId]

      -- Group foods by meal ID
      let foodMap = buildFoodMap allFoods

      -- Combine meals with their foods
      return $
        map
          ( \meal@(Entity mealId _) ->
              (meal, Map.findWithDefault [] mealId foodMap)
          )
          meals
  where
    -- Build a map from MealId to list of Food entities
    buildFoodMap :: [Entity Food] -> Map MealId [Entity Food]
    buildFoodMap foods =
      Map.fromListWith (++) [(foodMealId (entityVal f), [f]) | f <- foods]

-- | Delete a food item by ID and remove the meal if it becomes empty
deleteFoodById :: (MonadIO m) => FoodId -> ReaderT SqlBackend m Bool
deleteFoodById foodId = do
  mFood <- get foodId
  case mFood of
    Nothing -> return False
    Just food -> do
      let mealId = foodMealId food
      delete foodId
      -- Check if meal has any remaining foods
      remainingFoods <- selectList [FoodMealId ==. mealId] []
      -- If no foods left, delete the meal
      when (null remainingFoods) $ do
        delete mealId
      return True
