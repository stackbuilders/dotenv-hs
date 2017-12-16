{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Parser where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), pure)
import Data.Functor ((<$>))
#endif

import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Configuration.Dotenv.Scheme.Types

parseEnvsWithScheme
  :: [(String, EnvType)]
  -> Either [ParseError Char Void] ()
parseEnvsWithScheme valuesAndTypes =
  let parsedEithers = parseEnvs <$> valuesAndTypes
      parseEnvs (val, type') = val `parseEnvAs` type'
    in if all isRight parsedEithers
          then Right ()
          else Left (lefts parsedEithers)

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
