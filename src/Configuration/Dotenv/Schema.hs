-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2018 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for loadSafeFile

module Configuration.Dotenv.Schema
  ( checkConfig
  , checkSchema
  , readSchema
  )
  where

import Control.Monad

import Data.List
import Data.Yaml (decodeFileEither, prettyPrintParseException)

import Configuration.Dotenv.Schema.Helpers
import Configuration.Dotenv.Schema.Parser
import Configuration.Dotenv.Schema.Types

readSchema :: FilePath -> IO [Env]
readSchema schemaFile = do
  eitherEnvConf <- decodeFileEither schemaFile
  case eitherEnvConf of
    Right envConfs -> return envConfs
    Left errorYaml -> error (prettyPrintParseException errorYaml)

checkSchema :: [Env] -> [Env]
checkSchema envConfs =
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
  :: ValidatorMap
  -> [(String, String)]
  -> [Env]
  -> IO ()
checkConfig mapFormat envvars envsWithType =
  let envsTypeAndValue   = joinEnvs envsWithType envvars
      valuesAndTypes     = matchValueAndType envsTypeAndValue
      dotenvsMissing     = filter required (missingDotenvs envsWithType envsTypeAndValue)
      schemaEnvsMissing  = missingSchemaEnvs envvars envsTypeAndValue
   in do
     unless (null dotenvsMissing)
       (error $ "The following envs: "
                  ++ showMissingDotenvs dotenvsMissing
                  ++ " must be in the dotenvs")
     unless (null schemaEnvsMissing)
       (error $ "The following envs: "
                  ++ showMissingSchemaEnvs schemaEnvsMissing
                  ++ " must be in your schema.yml")
     case parseEnvsWithSchema mapFormat valuesAndTypes of
       Left errors -> error (unlines errors)
       _ -> return ()
