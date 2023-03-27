module Main (main) where

import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

import Server.Handlers (tradingServer)
import Server.Types (TradingEnv (TradingEnv))
import Trading.API (tradingAPI)

main :: IO ()
main = do
  envRef <- newIORef initEnv
  run 8081 (serve tradingAPI $ tradingServer envRef)
  where
    initEnv = TradingEnv mempty mempty
