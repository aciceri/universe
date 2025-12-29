module Database.Utils where

import Database.Operations (FoodInput (..))
import LLM.Types (FoodData (..))

-- | Convert LLM FoodData to Database FoodInput
foodDataToInput :: FoodData -> FoodInput
foodDataToInput fd =
  FoodInput
    { fiName = foodName fd,
      fiCalories = foodCalories fd,
      fiProtein = foodProtein fd,
      fiCarbs = foodCarbs fd,
      fiFats = foodFats fd,
      fiFiber = foodFiber fd,
      fiSugars = foodSugars fd,
      fiNotes = foodNotes fd
    }
