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

{-# LANGUAGE CPP #-}

module Configuration.Dotenv
  ( load
  , loadFile
  , parseFile
  , onMissingFile )
 where

import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import System.IO.Error (isDoesNotExistError)
import Text.Megaparsec (parse)
import System.Environment.Dotenv (setEnv)

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
    Right options -> liftIO $ interpolateParsedVariables options

applySetting :: MonadIO m => Bool -> (String, String) -> m ()
applySetting override (key, value) = liftIO $ setEnv key value override

-- | The helper allows to avoid exceptions in the case of missing files and
-- perform some action instead.
--
-- @since 0.3.1.0

onMissingFile :: MonadCatch m
  => m a -- ^ Action to perform that may fail because of missing file
  -> m a               -- ^ Action to perform if file is indeed missing
  -> m a
onMissingFile f h = catchIf isDoesNotExistError f (const h)
