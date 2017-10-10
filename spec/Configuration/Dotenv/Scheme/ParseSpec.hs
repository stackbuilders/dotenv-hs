{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.Scheme.ParseSpec (spec) where

import Data.Yaml (decodeFileEither)
import Test.Hspec

import Configuration.Dotenv.Scheme.Types
import Configuration.Dotenv.Scheme.Parser

fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right v) = v
fromRight (Left v) = error ("Left " ++ show v ++ " constructor instead" )

spec :: Spec
spec = do
  specInstance
  specParse

specInstance :: Spec
specInstance = describe "parse a config env file" $
  it "parses the env config values from a file" $
    let expected :: Config
        expected = Config [EnvConf EnvBool "DOTENV", EnvConf EnvInteger "PORT"]
     in do
       actual <- decodeFileEither "spec/fixtures/.scheme.yml"
       fromRight actual `shouldBe` expected

specParse :: Spec
specParse =
  describe "parseTypeEnv" $
    context "given an integer" $ do
      context "when the integer can be parsed" $
        it "should return Right True" $
          let varContent = "123"
              integer = EnvInteger
           in varContent `isParseableAs` integer `shouldBe` True

      context "when the integer can't be parsed" $
        it "should return Left (ParseError Char Void)" $
          let varContent = "123x"
              integer = EnvInteger
           in varContent `isParseableAs` integer `shouldBe` True
