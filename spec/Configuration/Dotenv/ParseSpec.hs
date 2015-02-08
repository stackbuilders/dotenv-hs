{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (spec) where

import Configuration.Dotenv.Parse (configParser)

import Test.Hspec (it, describe, shouldBe, Spec)

import Text.Parsec (ParseError, parse)
import Text.ParserCombinators.Parsec.Error(errorMessages)


spec :: Spec
spec = describe "parse" $ do
  it "parses unquoted values" $
    parseConfig "FOO=bar" `shouldBe` Right [("FOO", "bar")]

  it "parses values with spaces around equal signs" $ do
    parseConfig "FOO =bar" `shouldBe` Right [("FOO", "bar")]
    parseConfig "FOO= bar" `shouldBe` Right [("FOO", "bar")]
    parseConfig "FOO =\t bar" `shouldBe` Right [("FOO", "bar")]

  it "parses double-quoted values" $
    parseConfig "FOO=\"bar\"" `shouldBe` Right [("FOO", "bar")]

  it "parses single-quoted values" $
    parseConfig "FOO='bar'" `shouldBe` Right [("FOO", "bar")]

  it "parses escaped double quotes" $
    parseConfig "FOO=\"escaped\\\"bar\"" `shouldBe`
    Right [("FOO", "escaped\"bar")]

  it "parses empty values" $
    parseConfig "FOO=" `shouldBe` Right [("FOO", "")]

  it "does not parse if line format is incorrect" $
    isLeft (parseConfig "lol$wut") `shouldBe` True

  it "expands newlines in quoted strings" $
    parseConfig "FOO=\"bar\nbaz\"" `shouldBe` Right [("FOO", "bar\nbaz")]

  it "parses variables with '.' in the name" $
    parseConfig "FOO.BAR=foobar" `shouldBe` Right [("FOO.BAR", "foobar")]

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


isLeft :: Either a b -> Bool
isLeft ( Left _ ) = True
isLeft _          = False


instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

parseConfig :: String -> Either ParseError [(String, String)]
parseConfig = parse configParser "(unknown)"
