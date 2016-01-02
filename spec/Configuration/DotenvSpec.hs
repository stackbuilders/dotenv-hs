{-# LANGUAGE CPP #-}

module Configuration.DotenvSpec (spec) where

import Configuration.Dotenv (load, loadFile)

import Test.Hspec

import System.Environment.Compat (lookupEnv, setEnv, unsetEnv)

{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "load" $ after_ (unsetEnv "foo") $ after_ (unsetEnv "bar") $ do
    it "loads the given list of configuration options to the environment" $ do
      lookupEnv "foo" `shouldReturn` Nothing

      load False [("foo", "bar")]

      lookupEnv "foo" `shouldReturn` Just "bar"

# ifdef mingw32_HOST_OS
# else
    it "loads the given list of empty configuration options to the environment" $ do
      lookupEnv "bar" `shouldReturn` Nothing

      load False [("bar", "")]

      lookupEnv "bar" `shouldReturn` Just ""
# endif

    it "preserves existing settings when overload is false" $ do
      setEnv "foo" "preset"

      load False [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "preset"

    it "overrides existing settings when overload is true" $ do
      setEnv "foo" "preset"

      load True [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "new setting"

  describe "loadFile" $ after_ (unsetEnv "DOTENV") $ after_ (unsetEnv "DOTEMPTYENV") $ do
    it "loads the configuration options to the environment from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"

# ifdef mingw32_HOST_OS
# else
    it "loads the empty configuration options to the environment from a file" $ do
      lookupEnv "DOTEMPTYENV" `shouldReturn` Nothing

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTEMPTYENV" `shouldReturn` Just ""
# endif

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset"

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "preset"

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset"

      loadFile True "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"
