{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.ParseSpec (main, spec) where

import Configuration.Dotenv.Internal 
  ( ParsedVariable (..),
    VarFragment (..),
    VarValue (..),
    configParser
  )
import Data.Void (Void)
import Test.Hspec (Spec, context, describe, hspec, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    arbitrary,
    arbitraryPrintableChar,
    choose,
    elements,
    forAll,
    listOf,
    listOf1,
    property,
    resize,
    sized,
    suchThat,
  )
import Text.Megaparsec (ParseErrorBundle, parse)

main :: IO ()
main = hspec $ do
  spec
  specProperty

spec :: Spec
spec = describe "parse" $ do
  it "supports CRLF line breaks" $
    parseConfig "FOO=bar\r\nbaz=fbb"
      `shouldParse` [ ParsedVariable "FOO" (Unquoted [VarLiteral "bar"]),
                      ParsedVariable "baz" (Unquoted [VarLiteral "fbb"])
                    ]

  it "parses empty values" $
    parseConfig "FOO="
      `shouldParse` [ParsedVariable "FOO" (Unquoted [])]

  it "parses unquoted interpolated values" $ do
    parseConfig "FOO=$HOME"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarInterpolation "HOME"])]
    parseConfig "FOO=abc_$HOME"
      `shouldParse` [ ParsedVariable
                        "FOO"
                        ( Unquoted
                            [ VarLiteral "abc_",
                              VarInterpolation "HOME"
                            ]
                        )
                    ]
    parseConfig "FOO=${HOME}"
      `shouldParse` [ParsedVariable "FOO" (Unquoted [VarInterpolation "HOME"])]
    parseConfig "FOO=abc_${HOME}"
      `shouldParse` [ ParsedVariable
                        "FOO"
                        ( Unquoted
                            [ VarLiteral "abc_",
                              VarInterpolation "HOME"
                            ]
                        )
                    ]

  it "parses double-quoted interpolated values" $ do
    parseConfig "FOO=\"$HOME\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarInterpolation "HOME"])]
    parseConfig "FOO=\"abc_$HOME\""
      `shouldParse` [ ParsedVariable
                        "FOO"
                        ( DoubleQuoted
                            [ VarLiteral "abc_",
                              VarInterpolation "HOME"
                            ]
                        )
                    ]
    parseConfig "FOO=\"${HOME}\""
      `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarInterpolation "HOME"])]
    parseConfig "FOO=\"abc_${HOME}\""
      `shouldParse` [ ParsedVariable
                        "FOO"
                        ( DoubleQuoted
                            [ VarLiteral "abc_",
                              VarInterpolation "HOME"
                            ]
                        )
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
      `shouldParse` [ ParsedVariable "foo" (Unquoted [VarLiteral "bar"]),
                      ParsedVariable "fizz" (Unquoted [VarLiteral "buzz"])
                    ]

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
      parseConfig "FOO=$(foo-bar arg1 arg2)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "foo-bar" ["arg1", "arg2"]])]

    it "parses a command with quoted arguments" $ do
      parseConfig "FOO=$(bin/foo \"arg 1\" arg2)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "bin/foo" ["arg 1", "arg2"]])]

    it "parses a command with arguments separated by newlines" $ do
      parseConfig "FOO=$(foo.sh \"arg 1\"\narg2\n)"
        `shouldParse` [ParsedVariable "FOO" (Unquoted [CommandInterpolation "foo.sh" ["arg 1", "arg2"]])]

  it "parses empty content (when the file is empty)" $
    parseConfig `shouldSucceedOn` ""

specProperty :: Spec
specProperty = do
  it "parses unquoted values as literals" $
    property $
      forAll (generateInput `suchThat` validChars) $ \input -> do
        let value = "FOO=" ++ input
        parseConfig value `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral input])]

  it "parses single-quoted values as literals" $
    property $
      forAll (generateInput `suchThat` validChars) $ \input -> do
        let quotedInput = "'" ++ input ++ "'"
        let value = "FOO=" ++ quotedInput
        parseConfig value `shouldParse` [ParsedVariable "FOO" (SingleQuoted [VarLiteral input])]

  it "parses double-quoted values as literals" $
    property $
      forAll (generateInput `suchThat` validChars) $ \input -> do
        let quotedInput = "\"" ++ input ++ "\""
        let value = "FOO=" ++ quotedInput
        parseConfig value `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral input])]

  it "parses escaped values as literals" $
    property $
      forAll (generateTextWithChar "\\\"" "\"") $ \input -> do
        let quotedInput = "\"" ++ fst input ++ "\""
        let value = "FOO=" ++ quotedInput
        parseConfig value `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral $ snd input])]

  it "parses # in quoted values" $
    property $
      forAll (generateTextWithChar "#" "#") $ \input -> do
        let quotedInput = "\"" ++ fst input ++ "\""
        let value = "FOO=" ++ quotedInput
        parseConfig value `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral $ snd input])]

  it "parses values with spaces around equal signs" $
    property $
      forAll generateValidInputWithSpaces $ \input -> do
        parseConfig (fst input) `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral $ snd input])]

  it "ignores inline comments after unquoted arguments" $
    property $
      forAll (generateInput `suchThat` validChars) $ \input -> do
        let value = "FOO=" ++ input ++ " # comment" ++ input
        parseConfig value `shouldParse` [ParsedVariable "FOO" (Unquoted [VarLiteral input])]

  it "ignores inline comments after quoted arguments" $
    property $
      forAll (generateInput `suchThat` validChars) $ \input -> do
        let value = "FOO=" ++ "\"" ++ input ++ "\"" ++ " # comment" ++ input
        parseConfig value `shouldParse` [ParsedVariable "FOO" (DoubleQuoted [VarLiteral input])]

---------------------------------------------------------------------------
-- Helpers

validChars :: String -> Bool
validChars str = not (null str) && all (`notElem` ['\\', '$', '\'', '"', '\"', ' ', '#']) str

-- | Generate random text with a specific character and expected output.
--
-- This function generates random text that includes a specific character and
-- produces both the input string and the expected output string. The generated
-- text is constructed by repeating a non-empty string multiple times and
-- appending it with the specified character. The size of the generated text is
-- limited by the 'maxSize' parameter.
generateTextWithChar
  -- | The character to include in the generated text
  :: String
  -- | The expected output character to include in the generated text
  -> String
  -> Gen (String, String)
generateTextWithChar input expected = do
  let maxSize = 5
  nonEmptyString <- resize maxSize $ generateInput `suchThat` validChars
  strForQuoted <- resize maxSize $ generateInput `suchThat` validChars
  numRepeat <- choose (1, maxSize)
  let quotedStr = input ++ strForQuoted
  let quotedStrForInput = expected ++ strForQuoted
  return
    ( concat (replicate numRepeat (nonEmptyString ++ quotedStr)),
      concat (replicate numRepeat (nonEmptyString ++ quotedStrForInput))
    )

generateValidInputWithSpaces :: Gen (String, String)
generateValidInputWithSpaces = do
  input <- generateInput `suchThat` validChars
  spacesBefore <- listOf (elements " \t")
  spacesAfter <- listOf (elements " \t")
  return ("FOO" ++ spacesBefore ++ "=" ++ spacesAfter ++ input, input)

generateInput :: Gen String
generateInput = do
  nonEmptyString <- arbitrary :: Gen NonEmptyPrintableString
  return (getNonEmptyPrintableString nonEmptyString)

-- Non-empty version of 'PrintableString'
newtype NonEmptyPrintableString = NonEmptyPrintableString
  { getNonEmptyPrintableString :: String
  }
  deriving (Show)

instance Arbitrary NonEmptyPrintableString where
  arbitrary = sized $ \s -> do
    someText <- resize (s * 10) $ listOf1 arbitraryPrintableChar
    return $ NonEmptyPrintableString someText

parseConfig :: String -> Either (ParseErrorBundle String Void) [ParsedVariable]
parseConfig = parse configParser ""
