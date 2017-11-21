{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), pure)
import Data.Functor ((<$>))
#endif

import Data.Either
import Data.Void
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

readScheme :: IO Config
readScheme = do
  eitherEnvConf <- decodeFileEither schemeFile
  case eitherEnvConf of
    Right envConf -> return envConf
    Left errorYaml -> error (prettyPrintParseException errorYaml)
  where schemeFile = ".scheme.yml"

checkEnvTypes
  :: [(String, String)]
  -> Config
  -> IO ()
checkEnvTypes envs schemeConfig =
  let prettyParsedErrors = unlines . fmap parseErrorPretty
   in case parseEnvsWithScheme schemeConfig envs of
        Left errors -> error (prettyParsedErrors errors)
        _ -> return ()

parseEnvsWithScheme
  :: Config
  -> [(String, String)]
  -> Either [ParseError Char Void] ()
parseEnvsWithScheme config envs =
  let envsWithConf = mapMatchVarWithType config envs
      parsedEnvs (v, t) = v `parseEnvAs` t
      parseEithers = parsedEnvs <$> envsWithConf
    in if all isRight parseEithers
          then Right ()
          else Left (lefts parseEithers)

parseEnvAs
  :: String -- ^ Value of the env variable
  -> EnvType -- ^ Type that the env variable should have
  -> Either (ParseError Char Void) ()
parseEnvAs envVal envTypeNeeded =
  parse dispatch "" envVal
    where
      errorMsg = "Couldn't parse " ++ envVal ++ " as " ++ show envTypeNeeded
      evalParse parser = parser *> eof *> pure () <|> fail errorMsg
      dispatch :: Parsec Void String ()
      dispatch =
        case envTypeNeeded of
          EnvInteger -> evalParse (many digitChar)
          EnvBool    -> evalParse (string "true" <|> string "false")
          EnvText    -> pure ()
