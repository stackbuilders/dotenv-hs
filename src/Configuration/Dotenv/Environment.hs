{-# LANGUAGE CPP #-}

module Configuration.Dotenv.Environment
  ( getEnvironment
  , lookupEnv
  , setEnv
  , unsetEnv
  ) where

#if MIN_VERSION_base(4,11,0)
import System.Environment.Blank (getEnvironment, unsetEnv)
import qualified System.Environment.Blank as Blank
#else
#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnvironment, setEnv, unsetEnv)
#else
import System.Environment.Compat (getEnvironment, setEnv, unsetEnv)
#endif
#endif

#if MIN_VERSION_base(4,11,0)
import System.Environment.Blank (getEnv)
#else
import System.Environment (lookupEnv)
#endif

#if MIN_VERSION_base(4,11,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv = getEnv

setEnv :: String -> String -> IO ()
setEnv name value = Blank.setEnv name value True
#endif
