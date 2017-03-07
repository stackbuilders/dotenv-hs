{- |
-- Module      :  Configuration.Dotenv
-- Copyright   :  © 2015–2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides the ability to load a dotenv file with an specific configuration.
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv
  ( loadFile
  , onMissingFile
  ) where

import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch, catchIf)
import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)
import System.IO.Error (isDoesNotExistError)
import System.IO.Error (isDoesNotExistError)
import Text.Megaparsec (parse)

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnv, setEnv, lookupEnv)
#else
import System.Environment.Compat (getEnv, setEnv, lookupEnv)
#endif

import Configuration.Dotenv.File
import Configuration.Dotenv.Types

-- | @loadFile@ parses the environment variables defined in the dotenv example
-- file and checks if they are defined in the dotenv file or in the environment.
-- It also allows to override the environment variables defined in the environment
-- with the values defined in the dotenv file.
loadFile :: MonadIO m => Config -> m [(String, String)]
loadFile Config{..} = do
  keys      <- map fst `liftM` parseFile configExamplePath
  maybeVars <- parseMaybeFile configPath
  setupEnvVars configOverride keys maybeVars

setupEnvVars :: MonadIO m => Bool -> [String] -> Maybe [(String, String)] -> m [(String, String)]
setupEnvVars override keys maybeVars =
  case maybeVars of
    Just vars -> setVariables override keys vars
    _         -> mapM getEnvVariable keys

getEnvVariable :: MonadIO m => String -> m (String, String)
getEnvVariable key =
  liftM ((,) key) (liftIO (getEnv key))

setVariables :: MonadIO m => Bool -> [String] -> [(String, String)] -> m [(String, String)]
setVariables override keys vars =
  mapM (getVariable vars) keys >>= mapM (setVariable override)

getVariable :: MonadIO m => [(String, String)] -> String -> m (String, String)
getVariable vars key =
  case lookup key vars of
    Just var -> return (key, var)
    _        -> getEnvVariable key

setVariable :: MonadIO m => Bool -> (String, String) -> m (String, String)
setVariable override var@(key, value) =
  if override then
    liftIO $ setEnv key value >> return var
  else do
    res <- liftIO $ lookupEnv key
    case res of
      Just val' -> return (key, val')
      _         -> liftIO $ setEnv key value >> return var


-- | The helper allows to avoid exceptions in the case of missing files and
-- perform some action instead.
--
-- @since 0.3.1.0

onMissingFile :: MonadCatch m
  => m a -- ^ Action to perform that may fail because of missing file
  -> m a -- ^ Action to perform if file is indeed missing
  -> m a
onMissingFile f h = catchIf isDoesNotExistError f (const h)
