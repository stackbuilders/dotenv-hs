{-# LANGUAGE CPP #-}

module Configuration.DotenvSpec (main, spec) where

import Configuration.Dotenv (load, loadFile, parseFile, onMissingFile)

import Test.Hspec

import System.Environment (lookupEnv)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif

import Environment.Portable (setEnv)
#if MIN_VERSION_base(4,7,0)
import System.Environment (unsetEnv)
#else
import System.Environment.Compat (unsetEnv)
#endif

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
      setEnv "foo" "preset" True

      load False [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "preset"

    it "overrides existing settings when overload is true" $ do
      setEnv "foo" "preset" True

      load True [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "new setting"

  describe "loadFile" $ after_ (unsetEnv "DOTENV") $ do
    it "loads the configuration options to the environment from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset" True

      loadFile False "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "preset"

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset" True

      loadFile True "spec/fixtures/.dotenv"

      lookupEnv "DOTENV" `shouldReturn` Just "true"

  describe "loadFile" $
    before_ (unsetEnv "BLANK") $
    after_ (unsetEnv "DOTENV") $ do
      it "can set blank variables" $ do
        lookupEnv "BLANK" `shouldReturn` Nothing
  
        loadFile True "spec/fixtures/.dotenv"
  
        lookupEnv "BLANK" `shouldReturn` Just ""

  describe "parseFile" $ after_ (unsetEnv "DOTENV") $ do
    it "returns variables from a file without changing the environment" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      (liftM head $ parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("DOTENV", "true")

      lookupEnv "DOTENV" `shouldReturn` Nothing

    it "recognizes unicode characters" $
      liftM (!! 1) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("UNICODE_TEST", "Manabí")

    it "recognises environment variables" $ do
      home <- fromMaybe "" <$> lookupEnv "HOME"
      liftM (!! 2) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("ENVIRONMENT", home)

    it "recognises previous variables" $
      liftM (!! 3) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("PREVIOUS", "true")

  describe "onMissingFile" $ after_ (unsetEnv "DOTENV") $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        onMissingFile (loadFile True "spec/fixtures/.dotenv") (return ())
        lookupEnv "DOTENV" `shouldReturn` Just "true"
    context "when target file is missing" $
      it "executes supplied handler instead" $
        onMissingFile (True <$ loadFile True "spec/fixtures/foo") (return False)
          `shouldReturn` False
