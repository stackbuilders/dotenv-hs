{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (spec) where

import Configuration.Dotenv.Parse (configParser)

import Test.Hspec (it, describe, shouldBe, Spec)

import Text.Parsec (ParseError, parse)

import Text.ParseErrorEq ()

spec :: Spec
spec = describe "parse" $ do
  it "parses unquoted values" $
    parseConfig "FOO=bar" `shouldBe` Right [("FOO", "bar")]

  it "parses values with spaces around equal signs" $ do
    parseConfig "FOO =bar" `shouldBe` Right [("FOO", "bar")]
    parseConfig "FOO= bar" `shouldBe` Right [("FOO", "bar")]
    parseConfig "FOO =\t bar" `shouldBe` Right [("FOO", "bar")]

  it "parses empty environmental variables" $
    parseConfig "FOO=\"\"" `shouldBe` Right [("FOO", "")]

  it "parses double-quoted values" $
    parseConfig "FOO=\"bar\"" `shouldBe` Right [("FOO", "bar")]

  it "parses single-quoted values" $
    parseConfig "FOO='bar'" `shouldBe` Right [("FOO", "bar")]

  it "parses escaped double quotes" $
    parseConfig "FOO=\"escaped\\\"bar\"" `shouldBe`
    Right [("FOO", "escaped\"bar")]

  it "parses empty values" $
    parseConfig "FOO=" `shouldBe` Right [("FOO", "")]

  it "does not parse if line format is incorrect" $ do
    isLeft (parseConfig "lol$wut") `shouldBe` True
    isLeft (parseConfig "KEY=\nVALUE") `shouldBe` True
    isLeft (parseConfig "KEY\n=VALUE") `shouldBe` True

  it "expands newlines in quoted strings" $
    parseConfig "FOO=\"bar\nbaz\"" `shouldBe` Right [("FOO", "bar\nbaz")]

  it "does not parse variables with hyphens in the name" $
    isLeft (parseConfig "FOO-BAR=foobar") `shouldBe` True

  it "parses variables with '_' in the name" $
    parseConfig "FOO_BAR=foobar" `shouldBe` Right [("FOO_BAR", "foobar")]

  it "parses variables with digits after the first character" $
    parseConfig "FOO_BAR_12=foobar" `shouldBe` Right [("FOO_BAR_12", "foobar")]

  it "allows vertical spaces after a quoted variable" $
    parseConfig "foo='bar' " `shouldBe` Right [("foo", "bar")]

  it "does not parse variable names beginning with a digit" $
    isLeft (parse configParser "null" "45FOO_BAR=foobar") `shouldBe` True

  it "strips unquoted values" $
    parseConfig "foo=bar " `shouldBe` Right [("foo", "bar")]

  it "ignores empty lines" $
    parseConfig "\n \t  \nfoo=bar\n \nfizz=buzz" `shouldBe`
    Right [("foo", "bar"), ("fizz", "buzz")]

  it "ignores inline comments after unquoted arguments" $
    parseConfig "FOO=bar # this is foo" `shouldBe` Right [("FOO", "bar")]

  it "ignores inline comments after quoted arguments" $
    parseConfig "FOO=\"bar\" # this is foo" `shouldBe` Right [("FOO", "bar")]

  it "allows # in quoted values" $
    parseConfig "foo=\"bar#baz\" # comment" `shouldBe`
    Right [("foo", "bar#baz")]

  it "ignores comment lines" $
    parseConfig "\n\t \n\n # HERE GOES FOO \nfoo=bar" `shouldBe`
    Right [("foo", "bar")]

  it "doesn't allow more configuration options after a quoted value" $
    isLeft (parse configParser "null" "foo='bar'baz='buz'") `shouldBe` True


isLeft :: Either a b -> Bool
isLeft ( Left _ ) = True
isLeft _          = False

parseConfig :: String -> Either ParseError [(String, String)]
parseConfig = parse configParser "(unknown)"
