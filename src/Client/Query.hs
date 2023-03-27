module Client.Query
  ( addCompany
  , addStocks
  , getPrice
  , getCount
  , buy
  , changePrice
  , cleanUp
  ) where

import Servant (NoContent, type (:<|>) ((:<|>)))
import Servant.Client (ClientM, client)

import Trading.API (tradingAPI)

addCompany :: Integer -> ClientM NoContent
addStocks :: Integer -> Integer -> ClientM NoContent
getPrice :: Integer -> ClientM Integer
getCount :: Integer -> ClientM Integer
buy :: Integer -> Integer -> ClientM Integer
changePrice :: Integer -> Integer -> ClientM NoContent
cleanUp :: ClientM NoContent
addCompany :<|> addStocks :<|> getPrice :<|> getCount :<|> buy :<|> changePrice :<|> cleanUp = client tradingAPI
