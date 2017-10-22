{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

import Control.Applicative
import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Types

import Debug.Trace
import Text.Pretty.Simple
import qualified Data.Text.Lazy as TL
tsid :: (Show a) => String -> a -> a
tsid msg x = trace (msg ++ (TL.unpack $ pShow x)) x

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
          EnvInteger -> evalParse (many digitChar *> eof)
          EnvBool    -> evalParse ((string "true" <|> string "false") *> eof)
          EnvText    -> pure True
