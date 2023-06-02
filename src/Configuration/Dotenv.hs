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
  ( -- * Dotenv Load Functions
    load
  , loadFile
  , parseFile
  , onMissingFile
      -- * Dotenv Types
  , module Configuration.Dotenv.Types
  ) where

import           Configuration.Dotenv.Environment    (getEnvironment, lookupEnv,
                                                      setEnv)
import           Configuration.Dotenv.Parse          (configParser)
import           Configuration.Dotenv.ParsedVariable (interpolateParsedVariables)
import           Configuration.Dotenv.Types          (Config (..),
                                                      defaultConfig)
import           Control.Exception                   (throw)
import           Control.Monad                       (unless, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class              (MonadIO (..))
import           Control.Monad.Reader                (ReaderT, ask, runReaderT)
import           Control.Monad.Trans                 (lift)
import           Data.Function                       (on)
import           Data.List                           (intercalate, union, (\\))
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NE
import           Data.Map                            (fromList, toList)
import           System.IO.Error                     (isDoesNotExistError)
import           Text.Megaparsec                     (errorBundlePretty, parse)

-- | Monad Stack for the application
type DotEnv m a = ReaderT Config m a

-- | Loads the given list of options into the environment. Optionally
-- override existing variables with values from Dotenv files.
load ::
     MonadIO m
  => Bool -- ^ Override existing settings?
  -> [(String, String)] -- ^ List of values to be set in environment
  -> m ()
load override kv =
  runReaderT (mapM_ applySetting (nubByLastVar kv)) defaultConfig {configOverride = override}

-- | @loadFile@ parses the environment variables defined in the dotenv example
-- file and checks if they are defined in the dotenv file or in the environment.
-- It also allows to override the environment variables defined in the environment
-- with the values defined in the dotenv file.
loadFile ::
     MonadIO m
  => Config -- ^ Dotenv configuration
  -> m ()
loadFile config@Config {..} = do
  environment <- liftIO getEnvironment

  vars <- case (NE.nonEmpty configPath, NE.nonEmpty configExamplePath) of
    (Nothing, _) -> pure []
    (Just envs, Nothing) -> concat <$> mapM parseFile envs
    (Just envs, Just envExamples) -> do
      readVars <- concat <$> mapM parseFile envs
      neededKeys <- map fst . concat <$> mapM parseFile envExamples

      let
        presentKeys = (union `on` map fst) environment readVars
        missingKeys = neededKeys \\ presentKeys

      pure $
        if null missingKeys
          then readVars
          else error $ concat
            [ "The following variables are present in "
            , showPaths "one of " envExamples
            , ", but not set in the current environment, or "
            , showPaths "any of " envs
            , ": "
            , intercalate ", " missingKeys
            ]

  unless allowDuplicates $ (lookUpDuplicates . map fst) vars
  if configDryRun
    then liftIO $ mapM_ (putStrLn . infoStr) vars
    else runReaderT (mapM_ applySetting (nubByLastVar vars)) config
 where
  showPaths :: String -> NonEmpty FilePath -> String
  showPaths _ (p:|[]) = p
  showPaths prefix ps = prefix <> intercalate ", " (NE.toList ps)

-- | Parses the given dotenv file and returns values /without/ adding them to
-- the environment.
parseFile ::
     MonadIO m
  => FilePath -- ^ A file containing options to read
  -> m [(String, String)] -- ^ Variables contained in the file
parseFile f = do
  contents <- liftIO $ readFile f
  case parse configParser f contents of
    Left e        -> error $ errorBundlePretty e
    Right options -> liftIO $ interpolateParsedVariables options

applySetting ::
     MonadIO m
  => (String, String) -- ^ A key-value pair to set in the environment
  -> DotEnv m (String, String)
applySetting kv@(k, v) = do
  Config {..} <- ask
  if configOverride
    then info kv >> setEnv'
    else do
      res <- lift . liftIO $ lookupEnv k
      case res of
        Nothing -> info kv >> setEnv'
        Just _  -> return kv
  where
    setEnv' = lift . liftIO $ setEnv k v >> return kv

-- | The function logs in console when a variable is loaded into the
-- environment.
info :: MonadIO m => (String, String) -> DotEnv m ()
info (key, value) = do
  Config {..} <- ask
  when (configVerbose || configDryRun) $
    lift . liftIO $
    putStrLn $ infoStr (key, value)

-- | The function prints out the variables
infoStr :: (String, String) -> String
infoStr (key, value) =  "[INFO]: Load env '" ++ key ++ "' with value '" ++ value ++ "'"

-- | The helper allows to avoid exceptions in the case of missing files and
-- perform some action instead.
--
-- @since 0.3.1.0
onMissingFile ::
     MonadCatch m
  => m a -- ^ Action to perform that may fail because of missing file
  -> m a -- ^ Action to perform if file is indeed missing
  -> m a
onMissingFile f h = catchIf isDoesNotExistError f (const h)

-- | The helper throws an exception if the allow duplicate is set to False.
forbidDuplicates :: MonadIO m => String -> m ()
forbidDuplicates key =
  throw $
  userError $
  "[ERROR]: Env '" ++
  key ++
  "' is duplicated in a dotenv file. Please, fix that (or remove --no-dups)."

lookUpDuplicates :: MonadIO m => [String] -> m ()
lookUpDuplicates [] = return ()
lookUpDuplicates [_] = return ()
lookUpDuplicates (x:xs) =
  if x `elem` xs
    then forbidDuplicates x
    else lookUpDuplicates xs

nubByLastVar :: [(String, String)] -> [(String, String)]
nubByLastVar = toList . fromList
