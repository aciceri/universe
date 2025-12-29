{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Config
import Data.Time (Day)
import Handlers.Chat
import Handlers.Confirm
import Handlers.DeleteFood
import Handlers.Stats
import Servant

type API =
  "api"
    :> ( "chat" :> ReqBody '[JSON] ChatRequest :> Post '[JSON] ChatResponse
           :<|> "chat" :> "confirm" :> ReqBody '[JSON] ConfirmRequest :> Post '[JSON] ConfirmResponse
           :<|> "meals" :> QueryParam "from" Day :> QueryParam "to" Day :> Get '[JSON] StatsResponse
           :<|> "foods" :> Capture "foodId" Int :> Delete '[JSON] DeleteFoodResponse
       )

api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = chatHandler config :<|> confirmHandler config :<|> statsHandler config :<|> deleteFoodHandler config
