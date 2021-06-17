{-# LANGUAGE CPP #-}

module Configuration.Dotenv.Environment
  ( getEnvironment
  , lookupEnv
  , setEnv
  , unsetEnv
  ) where

#if MIN_VERSION_base(4,11,0)
import System.Environment.Blank (getEnvironment, getEnv, unsetEnv)
import qualified System.Environment.Blank as Blank
#else
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
#endif

#if MIN_VERSION_base(4,11,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv = getEnv

setEnv :: String -> String -> IO ()
setEnv name value = Blank.setEnv name value True
#endif
