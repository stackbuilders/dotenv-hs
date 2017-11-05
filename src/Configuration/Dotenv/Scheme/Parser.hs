{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), (<|>))
import Data.Functor ((<$>), pure)
#endif

import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

areParseable
  :: Config
  -> [(String, String)]
  -> Either [ParseError Char Void] ()
areParseable config envs =
  let envsWithConf = mapMatchEnvWithConf config envs
      parsedEnvs (v, t) = v `isParseableAs` t
      eithers = parsedEnvs <$> envsWithConf
    in if all isRight eithers
          then Right ()
          else Left (lefts eithers)

isParseableAs
  :: String -- ^ Value of the env variable
  -> EnvType -- ^ Type that the env variable should have
  -> Either (ParseError Char Void) ()
isParseableAs envVal envTypeNeeded =
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
