-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for loadSafeFile

module Configuration.Dotenv.Scheme
  ( checkConfig
  , checkScheme
  , readScheme
  )
  where

import           Control.Monad

import           Data.List
import           Data.Yaml                           (decodeFileEither,
                                                      prettyPrintParseException)

import           Configuration.Dotenv.Scheme.Helpers
import           Configuration.Dotenv.Scheme.Parser
import           Configuration.Dotenv.Scheme.Types

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
  :: ValidatorMap
  -> [(String, String)]
  -> [Env]
  -> IO ()
checkConfig mapFormat envvars envsWithType =
  let envsTypeAndValue   = joinEnvs envsWithType envvars
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
     case parseEnvsWithScheme mapFormat valuesAndTypes of
       Left errors -> error (unlines errors)
       _           -> return ()
