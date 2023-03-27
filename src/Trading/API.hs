module Trading.API
  ( TradingAPI
  , tradingAPI
  ) where

import Data.Data (Proxy (..))
import Servant.API
  (Get, JSON, Post, PostNoContent, QueryParam', Required, Strict, type (:<|>), type (:>))

type RequiredParam = QueryParam' '[Required, Strict]

type TradingAPI
  =    "add_company" :> RequiredParam "id" Integer :> PostNoContent
  :<|> "add_stocks" :> RequiredParam "id" Integer :> RequiredParam "count" Integer :> PostNoContent
  :<|> "get_price" :> RequiredParam "id" Integer :> Get '[JSON] Integer
  :<|> "get_count" :> RequiredParam "id" Integer :> Get '[JSON] Integer
  :<|> "buy" :> RequiredParam "id" Integer :> RequiredParam "count" Integer :> Post '[JSON] Integer
  :<|> "change_price" :> RequiredParam "id" Integer :> RequiredParam "new_price" Integer :> PostNoContent
  :<|> "clean_up" :> PostNoContent

tradingAPI :: Proxy TradingAPI
tradingAPI = Proxy
