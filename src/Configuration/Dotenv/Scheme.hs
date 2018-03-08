module Configuration.Dotenv.Scheme
  ( checkConfig
  , checkScheme
  , loadSafeFile
  , runSchemaChecker
  )
  where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))

import Data.List
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Text.Megaparsec
import System.Directory (doesFileExist)

import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (Config(..))
import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Parser
import Configuration.Dotenv.Scheme.Types

-- | @loadSafeFile@ parses the /.scheme.yml/ file and will perform the type checking
-- of the environment variables in the /.env/ file.
loadSafeFile
  :: MonadIO m
  => FilePath
  -> Config
  -> m [(String, String)]
loadSafeFile schemaFile config = do
  envs <- loadFile config
  liftIO (readScheme schemaFile >>= checkConfig envs . checkScheme)
  return envs

readScheme :: FilePath -> IO [Env]
readScheme schemeFile = do
  eitherEnvConf <- decodeFileEither schemeFile
  case eitherEnvConf of
    Right envConfs -> return envConfs
    Left errorYaml -> error (prettyPrintParseException errorYaml)

checkScheme :: [Env] -> [Env]
checkScheme envConfs =
  case duplicatedConfs of
    []   -> envConfs
    dups -> error (duplicatedConfErrorMsg $ uniqueConfs dups)
  where
    duplicatedConfs  = deleteFirstsBy confEquals envConfs (uniqueConfs envConfs)
    uniqueConfs      = nubBy confEquals
    a `confEquals` b = envName a == envName b

duplicatedConfErrorMsg :: [Env] -> String
duplicatedConfErrorMsg = ("Duplicated env variable configuration in schema: " ++) . showMissingDotenvs

checkConfig
  :: [(String, String)]
  -> [Env]
  -> IO ()
checkConfig envvars envsWithType =
  let prettyParsedErrors = unlines . fmap parseErrorTextPretty
      envsTypeAndValue   = joinEnvs envsWithType envvars
      valuesAndTypes     = matchValueAndType envsTypeAndValue
      dotenvsMissing     = filter required (missingDotenvs envsWithType envsTypeAndValue)
      schemeEnvsMissing  = missingSchemeEnvs envvars envsTypeAndValue
   in do
     unless (null dotenvsMissing)
       (error $ "The following envs: "
                  ++ showMissingDotenvs dotenvsMissing
                  ++ " must be in the dotenvs")
     unless (null schemeEnvsMissing)
       (error $ "The following envs: "
                  ++ showMissingSchemeEnvs schemeEnvsMissing
                  ++ " must be in your scheme.yml")
     case parseEnvsWithScheme valuesAndTypes of
        Left errors -> error (prettyParsedErrors errors)
        _ -> return ()

runSchemaChecker
  :: FilePath
  -> Config
  -> IO ()
runSchemaChecker schemeFile config = do
  exists <- doesFileExist schemeFile
  when exists
     (void $ loadSafeFile schemeFile config)
