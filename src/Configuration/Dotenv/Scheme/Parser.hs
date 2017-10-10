{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

import Control.Applicative
import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Types

isParseableAs
  :: String -- ^ Value of the env variable
  -> EnvType -- ^ Type that the env variable should have
  -> Bool
isParseableAs envVal envTypeNeeded =
  isRight $ parse dispatch "" envVal
    where
      errorMsg = "Couldn't parse " ++ envVal ++ " as " ++ show envTypeNeeded
      evalParse parser = parser *> pure True <|> fail errorMsg
      dispatch :: Parsec Void String Bool
      dispatch =
        case envTypeNeeded of
          EnvInteger -> evalParse digitChar
          EnvBool    -> evalParse (string "true" <|> string "false")
          EnvText    -> pure True
