import Control.Monad (void)
import Control.Monad.Catch (try)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Bifunctor (first)
import Data.IORef (newIORef)
import Data.List (sortBy)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Servant.Client (BaseUrl (..), ClientError, Scheme (Http), mkClientEnv)

import Client.Interop (addMoney, allCash, buyStocks, createUser, getStocks, queryStocks)
import Client.Query (addCompany, addStocks, changePrice, cleanUp)
import Client.Types (PersonalEnv (PersonalEnv), PersonalM, Stocks (..))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, (@=?))
import TestContainers
  (TestContainer, containerPort, containerRequest, fromBuildContext, run, setExpose, setWaitingFor,
  waitUntilMappedPortReachable, (&))
import TestContainers.Tasty (withContainers)

setupContainers :: TestContainer BaseUrl
setupContainers = do
  container <- run $
    containerRequest (fromBuildContext "." Nothing)
      & setExpose [8081]
      & setWaitingFor (waitUntilMappedPortReachable 8081)

  pure BaseUrl
    { baseUrlScheme = Http
    , baseUrlHost = "127.0.0.1"
    , baseUrlPort = containerPort container 8081
    , baseUrlPath = ""
    }

clientTest :: IO BaseUrl -> PersonalM a -> Assertion
clientTest ioUrl act = do
  manager <- newManager tlsManagerSettings
  url <- ioUrl

  let clientEnv = mkClientEnv manager url
  let initEnv = PersonalEnv mempty mempty clientEnv

  envRef <- newIORef initEnv
  void $ runReaderT (act >> queryStocks cleanUp) envRef

tests :: TestTree
tests = withContainers setupContainers \setup ->
  testGroup "Container tests"
    [ testCase "Buy stocks and change the price" do
        clientTest setup do
          queryStocks (addCompany 1)
          queryStocks (addStocks 1 42)

          createUser 1
          addMoney 1 1000

          buyStocks 1 1 5

          queryStocks (changePrice 1 200)
          cash <- allCash 1

          liftIO $ assertEqual "Cash should be 1500" 1500 cash

    , testCase "Buy stocks from non-existing company" do
        clientTest setup do
          createUser 1

          try @_ @ClientError (buyStocks 1 1 1) >>= \case
            Left{} -> pure ()
            Right{} -> liftIO $ assertFailure "Company doesn't have stocks"

    , testCase "Buy stocks more than company has" do
        clientTest setup do
          queryStocks (addCompany 1)
          queryStocks (addStocks 1 5)

          createUser 1
          addMoney 1 1000

          bought <- buyStocks 1 1 10
          liftIO $ assertEqual "Company has only 5 stocks" 5 bought

    , testCase "All stocks" do
        clientTest setup do
          queryStocks (addCompany 1)
          queryStocks (addCompany 2)
          queryStocks (addCompany 3)

          queryStocks (addStocks 1 10)
          queryStocks (addStocks 2 10)
          queryStocks (addStocks 3 10)

          createUser 1
          addMoney 1 100000

          buyStocks 1 1 2
          buyStocks 1 2 4
          buyStocks 1 3 6

          stocksAndPrice <- getStocks 1

          let actual = stocksAndPrice
                & sortBy do \(_, lhs) (_, rhs) -> compare lhs rhs
                & map (first \Stocks{..} -> (sFromCompany, sQuantity))

          let expected =
                [ ((1, 2), 200)
                , ((2, 4), 400)
                , ((3, 6), 600)
                ]

          liftIO (expected @=? actual)
    ]

main :: IO ()
main = defaultMain tests
