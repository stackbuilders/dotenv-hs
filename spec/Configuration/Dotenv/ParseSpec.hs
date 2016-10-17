{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (main, spec) where

import Configuration.Dotenv.Parse (configParser)
import Test.Hspec (it, describe, Spec, hspec)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Text.Megaparsec (ParseError, Dec, parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parse" $ do
  it "parses unquoted values" $
    parseConfig "FOO=bar" `shouldParse` [("FOO", "bar")]

  it "parses values with spaces around equal signs" $ do
    parseConfig "FOO =bar" `shouldParse` [("FOO", "bar")]
    parseConfig "FOO= bar" `shouldParse` [("FOO", "bar")]
    parseConfig "FOO =\t bar" `shouldParse` [("FOO", "bar")]

  it "parses double-quoted values" $
    parseConfig "FOO=\"bar\"" `shouldParse` [("FOO", "bar")]

  it "parses single-quoted values" $
    parseConfig "FOO='bar'" `shouldParse` [("FOO", "bar")]

  it "parses escaped double quotes" $
    parseConfig "FOO=\"escaped\\\"bar\""
      `shouldParse` [("FOO", "escaped\"bar")]

  it "supports CRLF line breaks" $
    parseConfig "FOO=bar\r\nbaz=fbb"
      `shouldParse` [("FOO", "bar"), ("baz", "fbb")]

  it "parses empty values" $
    parseConfig "FOO=" `shouldParse` [("FOO", "")]

  it "does not parse if line format is incorrect" $ do
    parseConfig `shouldFailOn` "lol$wut"
    parseConfig `shouldFailOn` "KEY=\nVALUE"
    parseConfig `shouldFailOn` "KEY\n=VALUE"

  it "expands newlines in quoted strings" $
    parseConfig "FOO=\"bar\nbaz\"" `shouldParse` [("FOO", "bar\nbaz")]

  it "does not parse variables with hyphens in the name" $
    parseConfig `shouldFailOn` "FOO-BAR=foobar"

  it "parses variables with \"_\" in the name" $
    parseConfig "FOO_BAR=foobar" `shouldParse` [("FOO_BAR", "foobar")]

  it "parses variables with digits after the first character" $
    parseConfig "FOO_BAR_12=foobar" `shouldParse` [("FOO_BAR_12", "foobar")]

  it "allows vertical spaces after a quoted variable" $
    parseConfig "foo='bar' " `shouldParse` [("foo", "bar")]

  it "does not parse variable names beginning with a digit" $
    parseConfig `shouldFailOn` "45FOO_BAR=foobar"

  it "strips unquoted values" $
    parseConfig "foo=bar " `shouldParse` [("foo", "bar")]

  it "ignores empty lines" $
    parseConfig "\n \t  \nfoo=bar\n \nfizz=buzz"
      `shouldParse` [("foo", "bar"), ("fizz", "buzz")]

  it "ignores inline comments after unquoted arguments" $
    parseConfig "FOO=bar # this is foo" `shouldParse` [("FOO", "bar")]

  it "ignores inline comments after quoted arguments" $
    parseConfig "FOO=\"bar\" # this is foo" `shouldParse` [("FOO", "bar")]

  it "allows \"#\" in quoted values" $
    parseConfig "foo=\"bar#baz\" # comment"
      `shouldParse` [("foo", "bar#baz")]

  it "ignores comment lines" $
    parseConfig "\n\t \n\n # HERE GOES FOO \nfoo=bar"
      `shouldParse` [("foo", "bar")]

  it "doesn't allow more configuration options after a quoted value" $
    parseConfig `shouldFailOn` "foo='bar'baz='buz'"

parseConfig :: String -> Either (ParseError Char Dec) [(String, String)]
parseConfig = parse configParser ""
