-- |
-- Module      :  Configuration.Dotenv
-- Copyright   :  © 2015–2016 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains common functions to load and read dotenv files.

module Configuration.Dotenv (load, loadFile, parseFile) where

import System.Environment.Compat (lookupEnv, setEnv)

import Configuration.Dotenv.Parse (configParser)

import Text.Megaparsec (parse)

import Control.Monad.IO.Class (MonadIO(..))

-- | Loads the given list of options into the environment. Optionally
-- override existing variables with values from Dotenv files.
load ::
  MonadIO m =>
  Bool -- ^ Override existing settings?
  -> [(String, String)] -- ^ List of values to be set in environment
  -> m ()
load override = mapM_ (applySetting override)

-- | Loads the options in the given file to the environment. Optionally
-- override existing variables with values from Dotenv files.
loadFile ::
  MonadIO m =>
  Bool        -- ^ Override existing settings?
  -> FilePath -- ^ A file containing options to load into the environment
  -> m ()
loadFile override f = load override =<< parseFile f

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  MonadIO m =>
  FilePath -- ^ A file containing options to read
  -> m [(String, String)] -- ^ Variables contained in the file
parseFile f = do
  contents <- liftIO $ readFile f

  case parse configParser f contents of
    Left e        -> error $ "Failed to read file" ++ show e
    Right options -> return options

applySetting :: MonadIO m => Bool -> (String, String) -> m ()
applySetting override (key, value) =
  if override then
    liftIO $ setEnv key value

  else do
    res <- liftIO $ lookupEnv key

    case res of
      Nothing -> liftIO $ setEnv key value
      Just _  -> return ()
