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

module Configuration.Dotenv.Scheme
  ( SchemaErrors(..)
  , checkConfig
  , checkScheme
  , readScheme
  , defaultValidatorMap
  )
  where

import Control.Monad
import Control.Exception

import Data.Typeable
import Data.List
import Data.Yaml (decodeFileEither, prettyPrintParseException)

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Parser
import Configuration.Dotenv.Scheme.Types


data SchemaErrors =
  InvalidYaml String
    | DuplicatedEnvs [Env]
    | MissingEnvsInDotenvs [Env]
    | MissingEnvsInSchema [(String, String)]
    | ParseEnvFailures [String]
  deriving (Eq, Show, Typeable)

instance Exception SchemaErrors

readScheme :: FilePath -> IO [Env]
readScheme schemeFile = do
  eitherEnvConf <- decodeFileEither schemeFile
  case eitherEnvConf of
    Right envConfs -> return envConfs
    Left errorYaml -> throw $ InvalidYaml (prettyPrintParseException errorYaml)

checkScheme :: [Env] -> [Env]
checkScheme envConfs =
  case duplicatedConfs of
    []   -> envConfs
    dups -> throw $ DuplicatedEnvs (uniqueConfs dups)
  where
    duplicatedConfs  = deleteFirstsBy confEquals envConfs (uniqueConfs envConfs)
    uniqueConfs      = nubBy confEquals
    a `confEquals` b = envName a == envName b

checkConfig
  :: ValidatorMap
  -> [(String, String)]
  -> [Env]
  -> IO ()
checkConfig mapFormat envvars allEnvSchemas =
  let envSchemas         = checkScheme allEnvSchemas
      envsTypeAndValue   = joinEnvs envSchemas envvars
      valuesAndTypes     = matchValueAndType envsTypeAndValue
      dotenvsMissing     = filter required (missingDotenvs envSchemas envsTypeAndValue)
      schemeEnvsMissing  = missingSchemeEnvs envvars envsTypeAndValue
   in do
     unless (null dotenvsMissing)
       (throw $ MissingEnvsInDotenvs dotenvsMissing)
     unless (null schemeEnvsMissing)
       (throw $ MissingEnvsInSchema schemeEnvsMissing)
     case parseEnvsWithScheme mapFormat valuesAndTypes of
       Left errors -> throw $ ParseEnvFailures errors
       _ -> return ()
