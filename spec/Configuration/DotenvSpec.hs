module Configuration.DotenvSpec (main, spec) where

import Configuration.Dotenv (load, loadFile, loadFile')

import Test.Hspec

import System.Environment.Compat (lookupEnv, setEnv, unsetEnv)

{-# ANN module "HLint: ignore Reduce duplication" #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "load" $ after_ (unsetEnv "foo") $ do
    it "loads the given list of configuration options to the environment" $ do
      lookupEnv "foo" `shouldReturn` Nothing

      load False [("foo", "bar")]

      lookupEnv "foo" `shouldReturn` Just "bar"

    it "preserves existing settings when overload is false" $ do
      setEnv "foo" "preset"

      load False [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "preset"

    it "overrides existing settings when overload is true" $ do
      setEnv "foo" "preset"

      load True [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "new setting"

  describe "loadFile" $ after_ (unsetEnv "DOTENV") $ do
    it "loads the configuration options to the environment from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset"

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "preset"

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset"

      loadFile True "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"

  describe "loadFile'" $ after_ (unsetEnv "DOTENV") $
    it "returns environments from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      loadFile' "spec/fixtures/.dotenv" `shouldReturn` [("DOTENV", "true")]
