module Client.Types
  ( Stocks (..)
  , fromCompany
  , quantity
  , PersonalEnv (..)
  , userMoney
  , stocks
  , clEnv
  , PersonalM
  , PersonalException (..)
  ) where

import Control.Exception (Exception)
import Control.Lens (abbreviatedFields, makeLensesWith)
import Control.Monad.Trans.Reader (ReaderT)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Text (Text)
import Servant.Client (ClientEnv)

data Stocks = Stocks
  { sFromCompany :: Integer
  , sQuantity :: Integer
  } deriving stock (Eq, Ord)
makeLensesWith abbreviatedFields ''Stocks

instance Show Stocks where
  show Stocks{..} = unwords
    [ "From company"
    , show sFromCompany
    , "have"
    , show sQuantity
    , "stocks"
    ]

data PersonalEnv = PersonalEnv
  { peUserMoney :: Map Integer Integer
  , peStocks :: Map Integer (Map Integer Integer)
  , peClEnv :: ClientEnv
  }
makeLensesWith abbreviatedFields ''PersonalEnv

type PersonalM = ReaderT (IORef PersonalEnv) IO

newtype PersonalException = PersonalException Text
  deriving newtype Show
  deriving anyclass (Exception)
