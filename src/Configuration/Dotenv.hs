module Configuration.Dotenv (load, loadFile, parseFile) where

import System.Environment.Compat (lookupEnv, setEnv)

import Configuration.Dotenv.Parse (configParser)

import Text.Megaparsec (parse)

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
loadFile override f = load override =<< parseFile f

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  FilePath -- ^ A file containing options to read
  -> IO [(String, String)] -- ^ Variables contained in the file
parseFile f = do
  contents <- readFile f

  case parse configParser f contents of
    Left e        -> error $ "Failed to read file" ++ show e
    Right options -> return options

applySetting :: Bool -> (String, String) -> IO ()
applySetting override (key, value) =
  if override then
    setEnv key value

  else do
    res <- lookupEnv key

    case res of
      Nothing -> setEnv key value
      Just _  -> return ()
