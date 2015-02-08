module Configuration.DotenvSpec (spec) where

import Configuration.Dotenv (load, loadFile)

import Test.Hspec (it, describe, shouldBe, Spec)

import System.Environment.Compat (lookupEnv, setEnv, unsetEnv)

{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "load" $ do
    it "loads the given list of configuration options to the environment" $ do
      beforeSet <- lookupEnv "foo"
      beforeSet `shouldBe` Nothing

      load False [("foo", "bar")]

      afterSet <- lookupEnv "foo"
      afterSet `shouldBe` Just "bar"

      unsetEnv "foo" -- unset tested vars to clean test state

    it "preserves existing settings when overload is false" $ do
      setEnv "foo" "preset"

      load False [("foo", "new setting")]

      afterSet <- lookupEnv "foo"
      afterSet `shouldBe` Just "preset"

      unsetEnv "foo" -- unset tested vars to clean test state

    it "overrides existing settings when overload is true" $ do
      setEnv "foo" "preset"

      load True [("foo", "new setting")]

      afterSet <- lookupEnv "foo"
      afterSet `shouldBe` Just "new setting"

      unsetEnv "foo" -- unset tested vars to clean test state

  describe "loadFile" $ do
    it "loads the configuration options to the environment from a file" $ do
      beforeSet <- lookupEnv "DOTENV"
      beforeSet `shouldBe` Nothing

      loadFile False "spec/fixtures/.dotenv"

      afterSet <- lookupEnv "DOTENV"
      afterSet `shouldBe` Just "true"

      unsetEnv "DOTENV" -- unset tested vars to clean test state

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset"

      loadFile False "spec/fixtures/.dotenv"

      afterSet <- lookupEnv "DOTENV"
      afterSet `shouldBe` Just "preset"

      unsetEnv "DOTENV" -- unset tested vars to clean test state

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset"

      loadFile True "spec/fixtures/.dotenv"

      afterSet <- lookupEnv "DOTENV"
      afterSet `shouldBe` Just "true"

      unsetEnv "DOTENV" -- unset tested vars to clean test state
