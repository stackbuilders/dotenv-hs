{-# LANGUAGE CPP #-}

module Configuration.Dotenv (load, loadFile) where

import System.Environment.Compat (lookupEnv, setEnv)

# ifdef mingw32_HOST_OS
# else
import qualified System.Posix.Env as Posix
# endif

import Configuration.Dotenv.Parse (configParser)

import Text.Parsec (parse)

-- | Loads the given list of options into the environment. Optionally
-- override existing variables with values from Dotenv files.
load ::
  Bool -- ^ Override existing settings?
  -> [(String, String)] -- ^ List of values to be set in environment
  -> IO ()
load override = mapM_ (applySetting override)

-- | Loads the options in the given file to the environment. Optionally
-- override existing variables with values from Dotenv files.
loadFile ::
  Bool        -- ^ Override existing settings?
  -> FilePath -- ^ A file containing options to load into the environment
  -> IO ()
loadFile override f = do
  contents <- readFile f

  case parse configParser f contents of
    Left e        -> error $ "Failed to read file" ++ show e
    Right options -> load override options


applySetting :: Bool -> (String, String) -> IO ()
applySetting override (key, value) =
  if override then
    setEnv key value

  else do
    res <- lookupEnv key

    case res of
      Nothing ->
# ifdef mingw32_HOST_OS
	  setEnv key value
# else
          case value of
            "" -> Posix.setEnv key value False
            _ -> setEnv key value
# endif
      Just _  -> return ()
