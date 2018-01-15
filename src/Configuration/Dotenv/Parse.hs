-- |
-- Module      :  Configuration.Dotenv.Parse
-- Copyright   :  © 2015–2016 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for files in dotenv format. These files generally consist of lines
-- with the form key=value. Comments and blank lines are also supported. More
-- information on the dotenv format can be found in the project README and the
-- test suite.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Parse (configParser) where

import           Configuration.Dotenv.ParsedVariable
#if MIN_VERSION_base(4,8,0)
import           Control.Applicative                 (empty, many, some, (<|>))
#else
import           Control.Applicative                 (empty, many, some, (*>),
                                                      (<$>), (<*), (<*>), (<|>))
#endif
import           Control.Monad                       (void)
import           Data.Void                           (Void)
import           Text.Megaparsec                     (Parsec, between, eof,
                                                      sepEndBy, (<?>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer          as L

type Parser = Parsec Void String

data QuoteType = SingleQuote | DoubleQuote

-- | Returns a parser for a Dotenv configuration file. Accepts key and value
-- arguments separated by @=@. Comments in all positions are handled
-- appropriately.
configParser :: Parser [ParsedVariable]
configParser = between scn eof (sepEndBy envLine (eol <* scn))

-- | Parse a single environment variable assignment.
envLine :: Parser ParsedVariable
envLine = ParsedVariable <$> (lexeme variableName <* lexeme (char '=')) <*> lexeme value

-- | Variables must start with a letter or underscore, and may contain
-- letters, digits or '_' character after the first character.
variableName :: Parser VarName
variableName = ((:) <$> firstChar <*> many otherChar) <?> "variable name"
  where
    firstChar = char '_'  <|> letterChar
    otherChar = firstChar <|> digitChar

-- | Value: quoted or unquoted.
value :: Parser VarValue
value = (quotedValue <|> unquotedValue) <?> "variable value"
  where
    quotedValue   = quotedWith SingleQuote <|> quotedWith DoubleQuote
    unquotedValue = Unquoted <$> (many $ fragment "\'\" \t\n\r")

-- | Parse a value quoted with given character.
quotedWith :: QuoteType -> Parser VarValue
quotedWith SingleQuote = SingleQuoted <$> (between (char '\'') (char '\'') $ many (literalValueFragment "\'\\"))
quotedWith DoubleQuote = DoubleQuoted <$> (between (char '\"') (char '\"') $ many (fragment "\""))

fragment :: [Char] -> Parser VarFragment
fragment charsToEscape =
  interpolatedValueCommandInterpolation
    <|> interpolatedValueVarInterpolation
    <|> literalValueFragment ('$' : '\\' : charsToEscape)

interpolatedValueVarInterpolation :: Parser VarFragment
interpolatedValueVarInterpolation = VarInterpolation <$>
                            ((between (symbol "${") (symbol "}") variableName) <|>
                            (char '$' >> variableName))
  where
    symbol                = L.symbol sc

interpolatedValueCommandInterpolation :: Parser VarFragment
interpolatedValueCommandInterpolation =
  CommandInterpolation
    <$> between (symbol "$(") (symbol ")") (many alphaNumChar)
    where
      symbol = L.symbol sc

literalValueFragment :: [Char] -> Parser VarFragment
literalValueFragment charsToEscape = VarLiteral <$> (some $ escapedChar <|> normalChar)
  where
    escapedChar = (char '\\' *> anyChar) <?> "escaped character"
    normalChar  = noneOf charsToEscape <?> "unescaped character"

----------------------------------------------------------------------------
-- Boilerplate and whitespace setup

-- | Lexeme wrapper that takes care of consuming of white space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

-- | Space consumer. Consumes all white space including comments, but never
-- consumes newlines.
sc :: Parser ()
sc = L.space (void spaceChar') skipLineComment empty
{-# INLINE sc #-}

-- | Just like 'sc' but also eats newlines.
scn :: Parser ()
scn = L.space (void spaceChar) skipLineComment empty
{-# INLINE scn #-}

-- | Just like 'spaceChar', but does not consume newlines.
spaceChar' :: Parser Char
spaceChar' = oneOf (" \t" :: String)
{-# INLINE spaceChar' #-}

-- | Skip line comment and stop before newline character without consuming
-- it.
skipLineComment :: Parser ()
#if MIN_VERSION_megaparsec(5,1,0)
skipLineComment = L.skipLineComment "#"
#else
skipLineComment = p >> void (manyTill anyChar n)
  where p = string "#"
        n = lookAhead (void newline) <|> eof
#endif
{-# INLINE skipLineComment #-}
