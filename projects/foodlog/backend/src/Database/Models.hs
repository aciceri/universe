{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Models where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Meal
    date Day
    mealType Text
    timestamp UTCTime
    UniqueMeal date mealType
    deriving Show Eq

Food
    mealId MealId
    name Text
    calories Double Maybe
    protein Double Maybe
    carbs Double Maybe
    fats Double Maybe
    fiber Double Maybe
    sugars Double Maybe
    notes Text Maybe
    timestamp UTCTime
    deriving Show Eq

Message
    content Text
    role Text
    timestamp UTCTime
    deriving Show Eq
|]
