module Configuration.Dotenv.Parse (configParser) where

import Text.Megaparsec ((<?>), anyChar, char, eof, manyTill, try)
import Text.Megaparsec.Text (Parser)
import Text.Megaparsec.Char
  (digitChar, letterChar, newline, noneOf, oneOf)

import Control.Applicative
import Prelude

import Data.Maybe (catMaybes)
import Control.Monad (liftM, liftM2)

import qualified Data.Text as T

-- | Returns a parser for a Dotenv configuration file.  Accepts key
-- and value arguments separated by "=".  Comments are allowed on
-- lines by themselves and on blank lines.
configParser :: Parser [(T.Text, T.Text)]
configParser = catMaybes <$> many envLine <* eof


envLine :: Parser (Maybe (T.Text, T.Text))
envLine = liftM T.pack (comment <|> blankLine) *> return Nothing <|> Just <$> optionLine

blankLine :: Parser String
blankLine = many verticalSpace <* newline <?> "blank line"

optionLine :: Parser (T.Text, T.Text)
optionLine = liftM2 (,)
  (many verticalSpace *> variableName <* variableValueSeparator)
  value

-- | Variables must start with a letter or underscore, and may contain
-- letters, digits or '_' character after the first character.
variableName :: Parser T.Text
variableName = liftM T.pack (liftM2 (:) (letterChar <|> char '_')
  (many (letterChar <|> char '_' <|> digitChar <?>
         unwords [ "valid non-leading shell variable character (alphanumeric,"
                 , "digit or underscore)" ])))

  <?> unwords [ "shell variable name (letter or underscore followed"
              , "by alphanumeric characters or underscores)" ]

value :: Parser T.Text
value = quotedValue <|> unquotedValue <?> "variable value"

quotedValue :: Parser T.Text
quotedValue = (quotedWith '\'' <|> quotedWith '\"')
  <* (comment *> return () <|> many verticalSpace *> endOfLineOrInput)
  <?> "variable value surrounded with single or double quotes"

unquotedValue :: Parser T.Text
unquotedValue =
  liftM T.pack (manyTill anyChar (comment <|> many verticalSpace <* endOfLineOrInput))

-- | Based on a commented-string parser in:
-- http://hub.darcs.net/navilan/XMonadTasks/raw/Data/Config/Lexer.hs
quotedWith :: Char -> Parser T.Text
quotedWith c = liftM T.pack (char c *> many chr <* (char c <?> "closing quote character"))

  where chr = esc <|> noneOf [c]
        esc = escape *> char c <?> "escape character"

comment :: Parser String
comment = try (many verticalSpace *> char '#')
          *> manyTill anyChar endOfLineOrInput
          <?> "comment"

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

variableValueSeparator :: Parser ()
variableValueSeparator =
  many verticalSpace *> (char '=' <?> "variable-value separator character (=)")
  *> many verticalSpace *> return ()

escape :: Parser Char
escape = char '\\'

verticalSpace :: Parser Char
verticalSpace = oneOf " \t"
