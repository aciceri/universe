{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.DeleteFood where

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database.Operations
import Database.Persist.Sqlite
import GHC.Generics
import Servant

data DeleteFoodResponse = DeleteFoodResponse
  { success :: Bool,
    message :: Text
  }
  deriving (Show, Generic)

instance ToJSON DeleteFoodResponse

deleteFoodHandler :: Config -> Int -> Handler DeleteFoodResponse
deleteFoodHandler config foodId = do
  deleted <- liftIO $ runSqlite (T.pack $ databasePath config) $ do
    deleteFoodById (toSqlKey $ fromIntegral foodId)

  if deleted
    then
      return
        DeleteFoodResponse
          { success = True,
            message = "Alimento cancellato con successo"
          }
    else
      throwError err404 {errBody = "Alimento non trovato"}
