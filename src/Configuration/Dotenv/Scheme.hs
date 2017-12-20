module Configuration.Dotenv.Scheme
  ( readScheme
  , checkConfig
  )
  where

import Control.Monad

import Data.Yaml (decodeFileEither, prettyPrintParseException)

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Parser
import Configuration.Dotenv.Scheme.Types

import Text.Megaparsec

readScheme :: IO Config
readScheme = do
  eitherEnvConf <- decodeFileEither schemeFile
  case eitherEnvConf of
    Right envConf -> return envConf
    Left errorYaml -> error (prettyPrintParseException errorYaml)
  where schemeFile = ".scheme.yml"

checkConfig
  :: [(String, String)]
  -> Config
  -> IO ()
checkConfig envvars (Config envConfs) =
  let prettyParsedErrors = unlines . fmap parseErrorTextPretty
      envsWithType       = genEnvsWithType envConfs
      envsTypeAndValue   = joinEnvs envsWithType envvars
      valuesAndTypes     = matchValueAndType envsTypeAndValue
      dotenvsMissing     = getRequired (missingDotenvs envsWithType envsTypeAndValue)
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
