module Configuration.Dotenv.Parse (configParser) where

import Data.Maybe (catMaybes)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec.Char (space, newline, oneOf, noneOf)
import Control.Monad (liftM2)
import Text.Parsec.Combinator (eof)
import Control.Applicative ((<*), (*>), (<$>))
import Text.Parsec ((<|>), many, try, manyTill, char, anyChar)

-- | Returns a parser for a Dotenv configuration file.
-- Accepts key and value arguments separated by "=".
-- Comments are allowed on lines by themselves and on
-- blank lines.
configParser :: Parser [(String, String)]
configParser = catMaybes <$> many envLine


envLine :: Parser (Maybe (String, String))
envLine = (comment <|> blankLine) *> return Nothing
          <|> Just <$> configurationOptionWithArguments

blankLine :: Parser ()
blankLine = many verticalSpace *> newline *> return ()

configurationOptionWithArguments :: Parser (String, String)
configurationOptionWithArguments = liftM2 (,)
  (many space *> manyTill1 anyChar keywordArgSeparator)
  argumentParser

argumentParser :: Parser String
argumentParser = quotedArgument <|> unquotedArgument

quotedArgument :: Parser String
quotedArgument = quotedWith '\'' <|> quotedWith '\"'

unquotedArgument :: Parser String
unquotedArgument = manyTill anyChar
                   (comment <|> many verticalSpace *> endOfLineOrInput)

-- | Based on a commented-string parser in:
-- http://hub.darcs.net/navilan/XMonadTasks/raw/Data/Config/Lexer.hs
quotedWith :: Char -> Parser String
quotedWith c = char c *> many chr <* char c

  where chr = esc <|> noneOf [c]
        esc = escape *> char c

comment :: Parser ()
comment = try (many verticalSpace *> char '#')
          *> manyTill anyChar endOfLineOrInput
          *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

keywordArgSeparator :: Parser ()
keywordArgSeparator =
  many verticalSpace *> char '=' *> many verticalSpace *> return ()

escape :: Parser Char
escape = char '\\'

verticalSpace :: Parser Char
verticalSpace = oneOf " \t"
