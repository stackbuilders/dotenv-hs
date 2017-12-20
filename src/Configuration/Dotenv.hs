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

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv
  ( load
  , loadFile
  , loadSafeFile
  , parseFile
  , onMissingFile )
 where

import Control.Monad (liftM)
import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)
import Configuration.Dotenv.Scheme (readScheme, checkConfig)
import Configuration.Dotenv.Types (Config(..))
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (union, intersectBy, unionBy)
import System.Environment (lookupEnv)
import System.IO.Error (isDoesNotExistError)
import Text.Megaparsec (parse, parseErrorPretty)

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnvironment, setEnv)
#else
import System.Environment.Compat (getEnvironment, setEnv)
#endif

-- | Loads the given list of options into the environment. Optionally
-- override existing variables with values from Dotenv files.
load ::
  MonadIO m =>
  Bool -- ^ Override existing settings?
  -> [(String, String)] -- ^ List of values to be set in environment
  -> m ()
load override = mapM_ (applySetting override)

-- | @loadSafeFile@ parses the /.scheme.yml/ file and will perform the type checking
-- of the environment variables in the /.env/ file.
loadSafeFile
  :: MonadIO m
  => Config
  -> m [(String, String)]
loadSafeFile config = do
  envs <- loadFile config
  liftIO (readScheme >>= checkConfig envs)
  return envs

-- | @loadFile@ parses the environment variables defined in the dotenv example
-- file and checks if they are defined in the dotenv file or in the environment.
-- It also allows to override the environment variables defined in the environment
-- with the values defined in the dotenv file.
loadFile
  :: MonadIO m
  => Config
  -> m [(String, String)]
loadFile Config{..} = do
  environment <- liftIO getEnvironment
  readedVars <- concat `liftM` mapM parseFile configPath
  neededVars <- concat `liftM` mapM parseFile configExamplePath
  let coincidences = (environment `union` readedVars) `intersectEnvs` neededVars
      cmpEnvs env1 env2 = fst env1 == fst env2
      intersectEnvs = intersectBy cmpEnvs
      unionEnvs = unionBy cmpEnvs
      vars =
        if (not . null) neededVars
          then
            if length neededVars == length coincidences
              then readedVars `unionEnvs` neededVars
              else error $ "Missing env vars! Please, check (this/these) var(s) (is/are) set:" ++ concatMap ((++) " " . fst) neededVars
          else readedVars
  mapM (applySetting configOverride) vars

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
  MonadIO m =>
  FilePath -- ^ A file containing options to read
  -> m [(String, String)] -- ^ Variables contained in the file
parseFile f = do
  contents <- liftIO $ readFile f

  case parse configParser f contents of
    Left e        -> error $ parseErrorPretty e
    Right options -> liftIO $ interpolateParsedVariables options

applySetting :: MonadIO m => Bool -> (String, String) -> m (String, String)
applySetting override (key, value) =
  if override
    then liftIO (setEnv key value) >> return (key, value)
    else do
      res <- liftIO $ lookupEnv key

      case res of
        Nothing -> liftIO $ setEnv key value >> return (key, value)
        Just _  -> return (key, value)

-- | The helper allows to avoid exceptions in the case of missing files and
-- perform some action instead.
--
-- @since 0.3.1.0

onMissingFile :: MonadCatch m
  => m a -- ^ Action to perform that may fail because of missing file
  -> m a               -- ^ Action to perform if file is indeed missing
  -> m a
onMissingFile f h = catchIf isDoesNotExistError f (const h)
