module Configuration.Dotenv.Parse (configParser) where

import Text.Parsec ((<|>), anyChar, char, many, manyTill, try)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Char
  (digit, letter, newline, noneOf, oneOf)

import Control.Applicative ((<*), (*>), (<$>))
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
blankLine = many verticalSpace <* newline

optionLine :: Parser (String, String)
optionLine = liftM2 (,)
  (many verticalSpace *> variableName <* keywordArgSeparator)
  argumentParser

-- | Variables must start with a letter or underscore, and may contain
-- letters, digits or '_' character after the first character.
variableName :: Parser String
variableName =
  liftM2 (:) (letter <|> char '_') (many (letter <|> char '_' <|> digit))

argumentParser :: Parser String
argumentParser = quotedArgument <|> unquotedArgument

quotedArgument :: Parser String
quotedArgument = quotedWith '\'' <|> quotedWith '\"'

unquotedArgument :: Parser String
unquotedArgument =
  manyTill anyChar (comment <|> many verticalSpace <* endOfLineOrInput)

-- | Based on a commented-string parser in:
-- http://hub.darcs.net/navilan/XMonadTasks/raw/Data/Config/Lexer.hs
quotedWith :: Char -> Parser String
quotedWith c = char c *> many chr <* char c

  where chr = esc <|> noneOf [c]
        esc = escape *> char c

comment :: Parser String
comment = try (many verticalSpace *> char '#')
          *> manyTill anyChar endOfLineOrInput

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

keywordArgSeparator :: Parser ()
keywordArgSeparator =
  many verticalSpace *> char '=' *> many verticalSpace *> return ()

escape :: Parser Char
escape = char '\\'

verticalSpace :: Parser Char
verticalSpace = oneOf " \t"
