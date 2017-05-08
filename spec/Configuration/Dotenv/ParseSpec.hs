{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (main, spec) where

import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (ParsedVariable(..), VValue(..), VFragment(..))
import Test.Hspec (it, describe, Spec, hspec)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Text.Megaparsec (ParseError, Dec, parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parse" $ do
  it "parses unquoted values" $
    parseConfig "FOO=bar" `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"])]

  it "parses values with spaces around equal signs" $ do
    parseConfig "FOO =bar" `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"])]
    parseConfig "FOO= bar" `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"])]
    parseConfig "FOO =\t bar" `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"])]

  it "parses double-quoted values" $
    parseConfig "FOO=\"bar\"" `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VLiteral "bar"])]

  it "parses single-quoted values" $
    parseConfig "FOO='bar'" `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VLiteral "bar"])]

  it "parses escaped double quotes" $
    parseConfig "FOO=\"escaped\\\"bar\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VLiteral "escaped\"bar"])]

  it "supports CRLF line breaks" $
    parseConfig "FOO=bar\r\nbaz=fbb"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"]), ParsedVariable "baz" (Unquoted [VLiteral "fbb"])]

  it "parses empty values" $
    parseConfig "FOO=" `shouldParse` [ParsedVariable "FOO" (Unquoted [])]

  it "does not parse if line format is incorrect" $ do
    parseConfig `shouldFailOn` "lol$wut"
    parseConfig `shouldFailOn` "KEY=\nVALUE"
    parseConfig `shouldFailOn` "KEY\n=VALUE"

  it "expands newlines in quoted strings" $
    parseConfig "FOO=\"bar\nbaz\"" `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VLiteral "bar\nbaz"])]

  it "does not parse variables with hyphens in the name" $
    parseConfig `shouldFailOn` "FOO-BAR=foobar"

  it "parses variables with \"_\" in the name" $
    parseConfig "FOO_BAR=foobar" `shouldParse` [ParsedVariable "FOO_BAR" (Unquoted [VLiteral "foobar"])]

  it "parses variables with digits after the first character" $
    parseConfig "FOO_BAR_12=foobar" `shouldParse` [ParsedVariable "FOO_BAR_12" (Unquoted [VLiteral "foobar"])]

  it "allows vertical spaces after a quoted variable" $
    parseConfig "foo='bar' " `shouldParse` [ParsedVariable "foo" (SingleQuoted [VLiteral "bar"])]

  it "does not parse variable names beginning with a digit" $
    parseConfig `shouldFailOn` "45FOO_BAR=foobar"

  it "strips unquoted values" $
    parseConfig "foo=bar " `shouldParse` [ParsedVariable "foo" (Unquoted [VLiteral "bar"])]

  it "ignores empty lines" $
    parseConfig "\n \t  \nfoo=bar\n \nfizz=buzz"
      `shouldParse` [ParsedVariable "foo" (Unquoted [VLiteral "bar"]), ParsedVariable "fizz" (Unquoted [VLiteral "buzz"])]

  it "ignores inline comments after unquoted arguments" $
    parseConfig "FOO=bar # this is foo" `shouldParse` [ParsedVariable "FOO" (Unquoted [VLiteral "bar"])]

  it "ignores inline comments after quoted arguments" $
    parseConfig "FOO=\"bar\" # this is foo" `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VLiteral "bar"])]

  it "allows \"#\" in quoted values" $
    parseConfig "foo=\"bar#baz\" # comment"
      `shouldParse` [ParsedVariable "foo" (DoubleQuoted [VLiteral "bar#baz"])]

  it "ignores comment lines" $
    parseConfig "\n\t \n\n # HERE GOES FOO \nfoo=bar"
      `shouldParse` [ParsedVariable "foo" (Unquoted [VLiteral "bar"])]

  it "doesn't allow more configuration options after a quoted value" $
    parseConfig `shouldFailOn` "foo='bar'baz='buz'"

parseConfig :: String -> Either (ParseError Char Dec) [ParsedVariable]
parseConfig = parse configParser ""
