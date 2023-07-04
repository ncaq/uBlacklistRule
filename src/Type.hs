module Type where

import           RIO
import           RIO.Process

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
