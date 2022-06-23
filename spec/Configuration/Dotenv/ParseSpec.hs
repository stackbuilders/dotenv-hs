{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (main, spec) where

import Configuration.Dotenv.Parse (configParser)
import Configuration.Dotenv.ParsedVariable (ParsedVariable(..),
                                            VarValue(..),
                                            VarFragment(..))
import Data.Void (Void)
import Test.Hspec (it, context, describe, Spec, hspec)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn, shouldSucceedOn)
import Text.Megaparsec (ParseErrorBundle, parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parse" $ do
  it "parses unquoted values" $
    parseConfig "FOO=bar"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"])]

  it "parses values with spaces around equal signs" $ do
    parseConfig "FOO =bar"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"])]
    parseConfig "FOO= bar"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"])]
    parseConfig "FOO =\t bar"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"])]

  it "parses double-quoted values" $
    parseConfig "FOO=\"bar\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "bar"])]

  it "parses single-quoted values" $
    parseConfig "FOO='bar'"
      `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral "bar"])]

  it "parses escaped double quotes" $
    parseConfig "FOO=\"escaped\\\"bar\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "escaped\"bar"])]

  it "supports CRLF line breaks" $
    parseConfig "FOO=bar\r\nbaz=fbb"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"]),
                     ParsedVariable "baz" (Unquoted [VarLiteral "fbb"])]

  it "parses empty values" $
    parseConfig "FOO="
      `shouldParse` [ParsedVariable "FOO" (Unquoted [])]

  it "parses unquoted interpolated values" $ do
    parseConfig "FOO=$HOME"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarInterpolation "HOME"])]
    parseConfig "FOO=abc_$HOME"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "abc_",
                                                     VarInterpolation "HOME"])
                    ]
    parseConfig "FOO=${HOME}"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarInterpolation "HOME"])]
    parseConfig "FOO=abc_${HOME}"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "abc_",
                                                     VarInterpolation "HOME"])
                    ]

  it "parses double-quoted interpolated values" $ do
    parseConfig "FOO=\"$HOME\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarInterpolation "HOME"])]
    parseConfig "FOO=\"abc_$HOME\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "abc_",
                                                         VarInterpolation "HOME"])
                    ]
    parseConfig "FOO=\"${HOME}\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarInterpolation "HOME"])]
    parseConfig "FOO=\"abc_${HOME}\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "abc_",
                                                         VarInterpolation "HOME"])
                    ]

  it "parses single-quoted interpolated values as literals" $ do
    parseConfig "FOO='$HOME'"
      `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral "$HOME"])]
    parseConfig "FOO='abc_$HOME'"
      `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral "abc_$HOME"])]
    parseConfig "FOO='${HOME}'"
      `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral "${HOME}"])]
    parseConfig "FOO='abc_${HOME}'"
      `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral "abc_${HOME}"])]

  it "does not parse if line format is incorrect" $ do
    parseConfig `shouldFailOn` "lol$wut"
    parseConfig `shouldFailOn` "KEY=\nVALUE"
    parseConfig `shouldFailOn` "KEY\n=VALUE"

  it "expands newlines in quoted strings" $
    parseConfig "FOO=\"bar\nbaz\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "bar\nbaz"])]

  it "does not parse variables with hyphens in the name" $
    parseConfig `shouldFailOn` "FOO-BAR=foobar"

  it "parses variables with \"_\" in the name" $
    parseConfig "FOO_BAR=foobar"
      `shouldParse` [ParsedVariable "FOO_BAR" (Unquoted [VarLiteral "foobar"])]

  it "parses variables with digits after the first character" $
    parseConfig "FOO_BAR_12=foobar"
      `shouldParse` [ParsedVariable "FOO_BAR_12" (Unquoted [VarLiteral "foobar"])]

  it "allows vertical spaces after a quoted variable" $
    parseConfig "foo='bar' "
      `shouldParse` [ParsedVariable "foo" (SingleQuoted [VarLiteral "bar"])]

  it "does not parse variable names beginning with a digit" $
    parseConfig `shouldFailOn` "45FOO_BAR=foobar"

  it "strips unquoted values" $
    parseConfig "foo=bar "
      `shouldParse` [ParsedVariable "foo" (Unquoted [VarLiteral "bar"])]

  it "ignores empty lines" $
    parseConfig "\n \t  \nfoo=bar\n \nfizz=buzz"
      `shouldParse` [ParsedVariable "foo" (Unquoted [VarLiteral "bar"]),
                     ParsedVariable "fizz" (Unquoted [VarLiteral "buzz"])]

  it "ignores inline comments after unquoted arguments" $
    parseConfig "FOO=bar # this is foo"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "bar"])]

  it "ignores inline comments after quoted arguments" $
    parseConfig "FOO=\"bar\" # this is foo"
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral "bar"])]

  it "allows \"#\" in quoted values" $
    parseConfig "foo=\"bar#baz\" # comment"
      `shouldParse` [ParsedVariable "foo" (DoubleQuoted [VarLiteral "bar#baz"])]

  it "ignores comment lines" $
    parseConfig "\n\t \n\n # HERE GOES FOO \nfoo=bar"
      `shouldParse` [ParsedVariable "foo" (Unquoted [VarLiteral "bar"])]

  it "doesn't allow more configuration options after a quoted value" $
    parseConfig `shouldFailOn` "foo='bar'baz='buz'"

  context "$(command) interpolation" $ do
    it "parses a simple command" $ do
      parseConfig "FOO=$(command)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "command" []])]

    it "parses a command anywhere in the value" $ do
      parseConfig "FOO=asdf_$(command)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral "asdf_", CommandInterpolation "command" []])]

    it "parses a command with arguments" $ do
      parseConfig "FOO=$(command arg1 arg2)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "command" ["arg1", "arg2"]])]

    it "parses a command with quoted arguments" $ do
      parseConfig "FOO=$(command \"arg 1\" arg2)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "command" ["arg 1", "arg2"]])]

    it "parses a command with arguments separated by newlines" $ do
      parseConfig "FOO=$(command \"arg 1\"\narg2\n)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "command" ["arg 1", "arg2"]])]

  it "parses empty content (when the file is empty)" $
    parseConfig `shouldSucceedOn` ""

parseConfig :: String -> Either (ParseErrorBundle String Void) [ParsedVariable]
parseConfig = parse configParser ""
