module LLM.Prompts where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import qualified Data.Text as T

-- Embed system prompt from file at compile time
systemPromptTemplate :: Text
systemPromptTemplate = $(embedStringFile "prompts/system.txt")

systemPrompt :: Text -> Text
systemPrompt currentTimestamp =
  T.replace "{CURRENT_TIME}" currentTimestamp systemPromptTemplate
