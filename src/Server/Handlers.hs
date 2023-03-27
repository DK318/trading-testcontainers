{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

module Server.Handlers
  ( tradingServer
  ) where

import Control.Lens (At (at), (%~), (&), (^.))
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map qualified as M
import Servant
  (Handler, NoContent (NoContent), Server, ServerError (errBody), hoistServer, throwError,
  type (:<|>) ((:<|>)))

import Trading.API (TradingAPI, tradingAPI)

import Servant.Server (err412)
import Server.Types (RIO, TradingEnv (TradingEnv), companiesPrice, companiesStocks)

type TradingHandler = ExceptT ServerError RIO

getEnv :: TradingHandler TradingEnv
getEnv = ask >>= liftIO . readIORef

putEnv :: TradingEnv -> TradingHandler ()
putEnv env = ask >>= liftIO . flip writeIORef env

addCompany :: Integer -> TradingHandler NoContent
addCompany n = do
  env <- getEnv

  let updatedEnv = env
        & companiesStocks %~ M.insertWith (flip const) n 0
        & companiesPrice %~ M.insertWith (flip const) n 100

  putEnv updatedEnv
  pure NoContent

addStocks :: Integer -> Integer -> TradingHandler NoContent
addStocks n toAdd = do
  env <- getEnv

  let updatedEnv = env
        & companiesStocks %~ M.adjust (+ toAdd) n

  putEnv updatedEnv
  pure NoContent

getPrice :: Integer -> TradingHandler Integer
getPrice n = do
  env <- getEnv

  case env ^. companiesPrice . at n of
    Just p -> pure p
    Nothing -> throwError $ err412 { errBody = "Company doesn't exist" }

getCount :: Integer -> TradingHandler Integer
getCount n = do
  env <- getEnv

  case env ^. companiesStocks . at n of
    Just p -> pure p
    Nothing -> throwError $ err412 { errBody = "Company doesn't exist" }

buy :: Integer -> Integer -> TradingHandler Integer
buy n cnt = do
  env <- getEnv
  available <- getCount n

  let updatedEnv = env
        & companiesStocks . at n %~ fmap (const $ max 0 (available - cnt))

  putEnv updatedEnv
  pure $ min available cnt

changePrice :: Integer -> Integer -> TradingHandler NoContent
changePrice n newPrice = do
  env <- getEnv

  let updatedEnv = env
        & companiesPrice . at n %~ fmap (const newPrice)

  putEnv updatedEnv
  pure NoContent

cleanUp :: TradingHandler NoContent
cleanUp = do
  let emptyEnv = TradingEnv mempty mempty
  putEnv emptyEnv
  pure NoContent

tradingServer :: IORef TradingEnv -> Server TradingAPI
tradingServer initEnv = hoistServer tradingAPI mapper
  (    addCompany
  :<|> addStocks
  :<|> getPrice
  :<|> getCount
  :<|> buy
  :<|> changePrice
  :<|> cleanUp
  )
  where
    mapper :: forall a. TradingHandler a -> Handler a
    mapper act = do
      liftIO (flip runReaderT initEnv $ runExceptT act) >>= \case
        Left err -> throwError err
        Right res -> pure res
