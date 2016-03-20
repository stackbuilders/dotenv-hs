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

module Configuration.Dotenv.Parse (configParser) where

import Text.Megaparsec ((<?>), anyChar, char, eof, manyTill, try)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char
  (digitChar, letterChar, eol, noneOf, oneOf)

import Control.Applicative
import Prelude

import Data.Maybe (catMaybes)
import Control.Monad (liftM2)

-- | Returns a parser for a Dotenv configuration file.  Accepts key
-- and value arguments separated by "=".  Comments are allowed on
-- lines by themselves and on blank lines.
configParser :: Parser [(String, String)]
configParser = catMaybes <$> many envLine <* eof


envLine :: Parser (Maybe (String, String))
envLine = (comment <|> blankLine) *> return Nothing <|> Just <$> optionLine

blankLine :: Parser String
blankLine = many verticalSpace <* eol <?> "blank line"

optionLine :: Parser (String, String)
optionLine = liftM2 (,)
  (many verticalSpace *> variableName <* variableValueSeparator)
  value

-- | Variables must start with a letter or underscore, and may contain
-- letters, digits or '_' character after the first character.
variableName :: Parser String
variableName = liftM2 (:) (letterChar <|> char '_')
  (many (letterChar <|> char '_' <|> digitChar <?>
         unwords [ "valid non-leading shell variable character (alphanumeric,"
                 , "digit or underscore)" ]))

  <?> unwords [ "shell variable name (letter or underscore followed"
              , "by alphanumeric characters or underscores)" ]

value :: Parser String
value = quotedValue <|> unquotedValue <?> "variable value"

quotedValue :: Parser String
quotedValue = (quotedWith '\'' <|> quotedWith '\"')
  <* (comment *> return () <|> many verticalSpace *> endOfLineOrInput)
  <?> "variable value surrounded with single or double quotes"

unquotedValue :: Parser String
unquotedValue =
  manyTill anyChar (comment <|> many verticalSpace <* endOfLineOrInput)

-- | Based on a commented-string parser in:
-- http://hub.darcs.net/navilan/XMonadTasks/raw/Data/Config/Lexer.hs
quotedWith :: Char -> Parser String
quotedWith c = char c *> many chr <* (char c <?> "closing quote character")

  where chr = esc <|> noneOf [c]
        esc = escape *> char c <?> "escape character"

comment :: Parser String
comment = try (many verticalSpace *> char '#')
          *> manyTill anyChar endOfLineOrInput
          <?> "comment"

endOfLineOrInput :: Parser ()
endOfLineOrInput = eol *> return () <|> eof

variableValueSeparator :: Parser ()
variableValueSeparator =
  many verticalSpace *> (char '=' <?> "variable-value separator character (=)")
  *> many verticalSpace *> return ()

escape :: Parser Char
escape = char '\\'

verticalSpace :: Parser Char
verticalSpace = oneOf " \t"
