{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

import Control.Applicative
import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

areParseable
  :: Config
  -> [(String, String)]
  -> Bool
areParseable config envs =
  let envsWithConf = mapMatchEnvWithConf config envs
      canBeParsed (v, t)= v `isParseableAs` t
   in all canBeParsed envsWithConf

isParseableAs
  :: String -- ^ Value of the env variable
  -> EnvType -- ^ Type that the env variable should have
  -> Bool
isParseableAs envVal envTypeNeeded =
  isRight $ parse dispatch "" envVal
    where
      errorMsg = "Couldn't parse " ++ envVal ++ " as " ++ show envTypeNeeded
      evalParse parser = parser *> eof *> pure True <|> fail errorMsg
      dispatch :: Parsec Void String Bool
      dispatch =
        case envTypeNeeded of
          EnvInteger -> evalParse (many digitChar)
          EnvBool    -> evalParse (string "true" <|> string "false")
          EnvText    -> pure True
