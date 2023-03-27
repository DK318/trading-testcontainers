{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

module Client.Interop
  ( createUser
  , addMoney
  , getStocks
  , allCash
  , buyStocks
  , sellStocks
  , queryStocks
  ) where

import Control.Lens (At (at), (%~), (&), (^.))
import Control.Monad (forM, unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.IORef (readIORef, writeIORef)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Servant.Client (ClientM, runClientM)

import Client.Query (addStocks, buy, getPrice)
import Client.Types
  (PersonalEnv (..), PersonalException (PersonalException), PersonalM, Stocks (..), clEnv, stocks,
  userMoney)

getEnv :: PersonalM PersonalEnv
getEnv = ask >>= liftIO . readIORef

putEnv :: PersonalEnv -> PersonalM ()
putEnv env = ask >>= liftIO . flip writeIORef env

queryStocks :: ClientM a -> PersonalM a
queryStocks act = do
  env <- getEnv
  let clientEnv = env ^. clEnv

  liftIO (runClientM act clientEnv) >>= \case
    Left err -> throwM err
    Right res -> pure res

createUser :: Integer -> PersonalM ()
createUser n = do
  env <- getEnv

  let updatedEnv = env
        & userMoney %~ M.insertWith (flip const) n 0
        & stocks %~ M.insertWith (flip const) n mempty

  putEnv updatedEnv

addMoney :: Integer -> Integer -> PersonalM ()
addMoney n toAdd = do
  env <- getEnv

  unless (M.member n $ peUserMoney env) do
    throwM (PersonalException "User doesn't exist")

  let updatedEnv = env
        & userMoney . at n %~ fmap (+ toAdd)

  putEnv updatedEnv

getStocksMap :: Integer -> PersonalM (Map Integer Integer)
getStocksMap n = do
  env <- getEnv
  maybe (throwM $ PersonalException "User doesn't exist") pure (env ^. stocks . at n)

getStocks :: Integer -> PersonalM [(Stocks, Integer)]
getStocks n = do
  stocksAndQty <- M.toList <$> getStocksMap n
  forM stocksAndQty \(sFromCompany, sQuantity) -> do
    res <- queryStocks (getPrice sFromCompany)
    pure (Stocks{..}, res * sQuantity)

userCash :: Integer -> PersonalM Integer
userCash n = do
  env <- getEnv
  maybe (throwM $ PersonalException "User doesn't exist") pure (env ^. userMoney . at n)

allCash :: Integer -> PersonalM Integer
allCash n = do
  currentCash <- userCash n

  stocksAndPrice <- getStocks n
  let stocksPrice = sum $ snd <$> stocksAndPrice

  pure (currentCash + stocksPrice)

buyStocks :: Integer -> Integer -> Integer -> PersonalM Integer
buyStocks n from cnt = do
  unless (cnt >= 0) do
    throwM (PersonalException "Quantity is negative")

  price <- queryStocks (getPrice from)
  currentCash <- userCash n

  when (price * cnt > currentCash) do
    throwM (PersonalException "Insufficient money")

  bought <- queryStocks (buy from cnt)
  addMoney n (-bought * price)

  env <- getEnv

  let updatedEnv = env
        & stocks . at n %~ fmap (M.insertWith (+) from bought)

  putEnv updatedEnv
  pure bought

sellStocks :: Integer -> Integer -> Integer -> PersonalM Integer
sellStocks n from cnt = do
  unless (cnt >= 0) do
    throwM (PersonalException "Quantity is negative")

  price <- queryStocks (getPrice from)
  stocksMp <- getStocksMap n

  let howMuch = fromMaybe 0 (stocksMp ^. at from)
  let canSell = min howMuch cnt

  queryStocks (addStocks from canSell)
  addMoney n (canSell * price)

  env <- getEnv

  let updatedEnv = env
        & stocks . at n %~ fmap (M.adjust (subtract canSell) from)

  putEnv updatedEnv
  pure canSell
