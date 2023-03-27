module Server.Types
  ( TradingEnv (..)
  , companiesStocks
  , companiesPrice
  , RIO
  ) where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Data.IORef (IORef)
import Control.Lens (makeLensesWith, abbreviatedFields)

data TradingEnv = TradingEnv
  { teCompaniesStocks :: Map Integer Integer
  , teCompaniesPrice :: Map Integer Integer
  } deriving stock (Eq, Ord, Show)
makeLensesWith abbreviatedFields ''TradingEnv

type RIO = ReaderT (IORef TradingEnv) IO
