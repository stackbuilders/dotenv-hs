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

{-# LANGUAGE CPP #-}

module Configuration.Dotenv.Parse (configParser) where

import Control.Applicative
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

-- | Returns a parser for a Dotenv configuration file. Accepts key and value
-- arguments separated by @=@. Comments in all positions are handled
-- appropriately.
configParser :: Parser [(String, String)]
configParser = between scn eof (sepEndBy1 envLine (eol <* scn))

-- | Parse a single environment variable assignment.
envLine :: Parser (String, String)
envLine = (,) <$> (lexeme variableName <* lexeme (char '=')) <*> lexeme value

-- | Variables must start with a letter or underscore, and may contain
-- letters, digits or '_' character after the first character.
variableName :: Parser String
variableName = ((:) <$> firstChar <*> many otherChar) <?> "variable name"
  where
    firstChar = char '_'  <|> letterChar
    otherChar = firstChar <|> digitChar

-- | Value: quoted or unquoted.
value :: Parser String
value = (quotedValue <|> unquotedValue) <?> "variable value"
  where
    quotedValue   = quotedWith '\'' <|> quotedWith '\"'
    unquotedValue = many (noneOf "\'\" \t\n\r")

-- | Parse a value quoted with given character.
quotedWith :: Char -> Parser String
quotedWith q = between (char q) (char q) (many $ escapedChar <|> normalChar)
  where
    escapedChar = (char '\\' *> anyChar) <?> "escaped character"
    normalChar  = noneOf (q : "\\") <?> "unescaped character"

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
spaceChar' = oneOf " \t"
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
