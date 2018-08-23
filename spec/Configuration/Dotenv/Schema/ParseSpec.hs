{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration.Dotenv.Schema.ParseSpec (spec) where

import Data.Yaml (decodeFileEither)
import Test.Hspec

import Configuration.Dotenv.Schema.Types

-- | Extract Right from Either
--
fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right v) = v
fromRight (Left v) = error ("Left " ++ show v ++ " constructor instead" )

-- |
--
spec :: Spec
spec =
  describe "parse a config env file" $
    it "parses the env config values from a file" $
      let expected :: [Env]
          expected =
            [ Env "DOTENV" (EnvType "bool") True
            , Env "OTHERENV" (EnvType "bool") False
            , Env "PORT" (EnvType "integer") True
            , Env "TOKEN" (EnvType "integer") False
            , Env "URL" (EnvType "text") True
            , Env "TWO" (EnvType "twoLetters") False
            ]
       in do
         actual <- decodeFileEither "spec/fixtures/.schema.yml"
         fromRight actual `shouldBe` expected
