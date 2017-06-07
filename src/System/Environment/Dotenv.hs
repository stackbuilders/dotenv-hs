{-# LANGUAGE CPP #-}

module System.Environment.Dotenv (setEnv) where

#ifdef mingw32_HOST_OS
import System.Environment (lookupEnv)
#if MIN_VERSION_base(4,7,0)
import qualified System.Environment as W (setEnv)
#else
import qualified System.Environment.Compat as W (setEnv)
#endif
#else
import System.Posix.Env (setEnv)
#endif

#ifdef mingw32_HOST_OS
setEnv :: String -> String -> Bool -> IO ()
setEnv key value override =
  if override then W.setEnv key value
  else do
    res <- lookupEnv key

    case res of
      Nothing -> W.setEnv key value
      Just _  -> return ()
#endif
