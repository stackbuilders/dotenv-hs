-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains common functions to load and read dotenv files.

{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv
  (
  -- * Dotenv Load Functions
    load
  , loadFile
  , loadSafeFile
  , parseFile
  , onMissingFile
  -- * Dotenv Types
  , module Configuration.Dotenv.Types
  , ValidatorMap
  , defaultValidatorMap
  )
 where

import           Configuration.Dotenv.Environment    (getEnvironment, lookupEnv,
                                                      setEnv)
import           Configuration.Dotenv.Parse          (configParser)
import           Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)
import           Configuration.Dotenv.Scheme
import           Configuration.Dotenv.Scheme.Types   (ValidatorMap,
                                                      defaultValidatorMap)
import           Configuration.Dotenv.Types          (Config (..),
                                                      defaultConfig)
import           Control.Monad                       (liftM, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.List                           (intersectBy, union,
                                                      unionBy)
import           System.Directory                    (doesFileExist)
import           System.IO.Error                     (isDoesNotExistError)
import           Text.Megaparsec                     (errorBundlePretty, parse)

-- | Loads the given list of options into the environment. Optionally
-- override existing variables with values from Dotenv files.
load ::
  MonadIO m =>
  Bool -- ^ Override existing settings?
  -> [(String, String)] -- ^ List of values to be set in environment
  -> m ()
load override = mapM_ (applySetting override)

-- | @loadFile@ parses the environment variables defined in the dotenv example
-- file and checks if they are defined in the dotenv file or in the environment.
-- It also allows to override the environment variables defined in the environment
-- with the values defined in the dotenv file.
loadFile
  :: MonadIO m
  => Config -- ^ Dotenv configuration
  -> m [(String, String)] -- ^ Environment variables loaded
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
    Left e        -> error $ errorBundlePretty e
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

-- | @loadSafeFile@ parses the /.scheme.yml/ file and will perform the type checking
-- of the environment variables in the /.env/ file.
loadSafeFile
  :: MonadIO m
  => ValidatorMap -- ^ Map with custom validations
  -> FilePath -- ^ Filepath for schema file
  -> Config -- ^ Dotenv configuration
  -> m [(String, String)] -- ^ Environment variables loaded
loadSafeFile mapFormat schemaFile config = do
  envs <- loadFile config
  exists <- liftIO $ doesFileExist schemaFile
  when exists $
    liftIO (readScheme schemaFile >>= checkConfig mapFormat envs . checkScheme)
  return envs
