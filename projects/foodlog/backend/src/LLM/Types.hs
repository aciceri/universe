module LLM.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics

-- Request to LLM
data LLMRequest = LLMRequest
  { model :: Text,
    messages :: [LLMMessage]
  }
  deriving (Show, Generic)

instance ToJSON LLMRequest

data LLMMessage = LLMMessage
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic)

instance ToJSON LLMMessage

-- Response from LLM
data LLMResponse = LLMResponse
  { choices :: [LLMChoice]
  }
  deriving (Show, Generic)

instance FromJSON LLMResponse

data LLMChoice = LLMChoice
  { message :: LLMMessage
  }
  deriving (Show, Generic)

instance FromJSON LLMChoice

instance FromJSON LLMMessage

-- Parsed response from LLM (what we expect)
data LLMParsedResponse = LLMParsedResponse
  { needsConfirmation :: Bool,
    responseMessage :: Text,
    mealType :: Maybe Text,
    mealDate :: Maybe Day,
    foods :: [FoodData]
  }
  deriving (Show, Generic)

instance FromJSON LLMParsedResponse where
  parseJSON = withObject "LLMParsedResponse" $ \v ->
    LLMParsedResponse
      <$> v .: "needs_confirmation"
      <*> v .: "message"
      <*> v .:? "meal_type"
      <*> v .:? "date"
      <*> v .: "foods"

data FoodData = FoodData
  { foodName :: Text,
    foodCalories :: Maybe Double,
    foodProtein :: Maybe Double,
    foodCarbs :: Maybe Double,
    foodFats :: Maybe Double,
    foodFiber :: Maybe Double,
    foodSugars :: Maybe Double,
    foodNotes :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON FoodData where
  parseJSON = withObject "FoodData" $ \v ->
    FoodData
      <$> v .: "name"
      <*> v .:? "calories"
      <*> v .:? "protein"
      <*> v .:? "carbs"
      <*> v .:? "fats"
      <*> v .:? "fiber"
      <*> v .:? "sugars"
      <*> v .:? "notes"

instance ToJSON FoodData where
  toJSON fd =
    object
      [ "name" .= foodName fd,
        "calories" .= foodCalories fd,
        "protein" .= foodProtein fd,
        "carbs" .= foodCarbs fd,
        "fats" .= foodFats fd,
        "fiber" .= foodFiber fd,
        "sugars" .= foodSugars fd,
        "notes" .= foodNotes fd
      ]
