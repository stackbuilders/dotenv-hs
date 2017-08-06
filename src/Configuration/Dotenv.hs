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

import Data.List (union, intersectBy, unionBy)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadCatch, catchIf)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Traversable as DT
import System.IO.Error (isDoesNotExistError)

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnv, getEnvironment, setEnv, lookupEnv)
#else
import System.Environment.Compat (getEnv, getEnvironment, setEnv, lookupEnv)
#endif

import Configuration.Dotenv.File
import Configuration.Dotenv.Types

-- | @loadFile@ parses the environment variables defined in the dotenv example
-- file and checks if they are defined in the dotenv file or in the environment.
-- It also allows to override the environment variables defined in the environment
-- with the values defined in the dotenv file.
loadFile :: MonadIO m => Config -> m [(String, String)]
loadFile Config{..} = do
  environment <- liftIO getEnvironment
  readedVars <- concat `liftM` DT.mapM parseFile configPath
  neededVars <- concat `liftM` DT.mapM parseFile configExamplePath
  let numFoundCoincidences = length $ (environment `union` readedVars) `intersectEnvs` neededVars
      numNeededVars = length neededVars
      cmpEnvs env1 env2 = fst env1 == fst env2
      intersectEnvs = intersectBy cmpEnvs
      unionEnvs = unionBy cmpEnvs
      vars =
        if configSafe
          then
            if numNeededVars == numFoundCoincidences
              then readedVars `unionEnvs` neededVars
              else error $ "Some env vars are not defined. Please, check this var(s) (is/are) set: " ++ concatMap ((++) " " . fst) neededVars
          else readedVars
      keys = map fst vars
  setVariables configOverride keys vars

setVariables :: MonadIO m => Bool -> [String] -> [(String, String)] -> m [(String, String)]
setVariables override keys vars =
  mapM (getVariable vars) keys >>= mapM (setVariable override)

getVariable :: MonadIO m => [(String, String)] -> String -> m (String, String)
getVariable vars key =
  case lookup key vars of
    Just var -> return (key, var)
    _        -> getEnvVariable key

getEnvVariable :: MonadIO m => String -> m (String, String)
getEnvVariable key = do
  var <- (liftIO . getEnv) key
  return (key, var)

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
