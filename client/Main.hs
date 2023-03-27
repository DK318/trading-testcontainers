module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Catch (Exception (displayException), SomeException, handle)
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef (newIORef)
import Network.HTTP.Client (newManager)
import Network.HTTP.Conduit (tlsManagerSettings)
import Servant.Client (BaseUrl (..), Scheme (Http), mkClientEnv)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Read (readMaybe)

import Client.Interop (addMoney, allCash, buyStocks, createUser, getStocks, sellStocks)
import Client.Types (PersonalEnv (..), PersonalM)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let url = BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = 8081
        , baseUrlPath = ""
        }

  let clientEnv = mkClientEnv manager url
  let initEnv = PersonalEnv mempty mempty clientEnv

  envRef <- newIORef initEnv
  runReaderT (runInputT defaultSettings loop) envRef
  where
    handler :: SomeException -> InputT PersonalM ()
    handler someExc = outputStrLn (displayException someExc) >> loop

    loop :: InputT PersonalM ()
    loop = handler `handle` do
      getInputLine "client> " >>= \case
        Nothing -> pure ()
        Just str -> do
          case words str of
            ["createUser", readMaybe -> Just n] -> lift $ createUser n

            ["addMoney", readMaybe -> Just n, readMaybe -> Just toAdd] -> lift $ addMoney n toAdd

            ["getStocks", readMaybe -> Just n] -> do
              stocksAndPrice <- lift $ getStocks n
              forM_ stocksAndPrice \(st, price) -> do
                liftIO $ print st
                outputStrLn $ " with price " <> show price

            ["allCash", readMaybe -> Just n] -> do
              cash <- lift $ allCash n
              outputStrLn $ "This user have " <> show cash <> " cash"

            ["buyStocks", readMaybe -> Just n, readMaybe -> Just from, readMaybe -> Just cnt] -> do
              bought <- lift $ buyStocks n from cnt
              outputStrLn $ "Bought " <> show bought

            ["sellStocks", readMaybe -> Just n, readMaybe -> Just from, readMaybe -> Just cnt] -> do
              sold <- lift $ sellStocks n from cnt
              outputStrLn $ "Sold " <> show sold

            _ -> outputStrLn "Unexpected command"

          loop
