{-# LANGUAGE CPP #-}

module Configuration.DotenvSpec (main, spec) where

import Configuration.Dotenv.Types (Config(..))
import Configuration.Dotenv (load, loadFile, parseFile, onMissingFile)

import Test.Hspec

import System.Process (readCreateProcess, shell)
import System.Environment (lookupEnv, getEnv)
import Control.Monad (liftM, void)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif

#if MIN_VERSION_base(4,7,0)
import System.Environment (setEnv, unsetEnv)
#else
import System.Environment.Compat (setEnv, unsetEnv)
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
      setEnv "foo" "preset"

      load False [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "preset"

    it "overrides existing settings when overload is true" $ do
      setEnv "foo" "preset"

      load True [("foo", "new setting")]

      lookupEnv "foo" `shouldReturn` Just "new setting"

  describe "loadFile" $ after_ (mapM_ unsetEnv ["DOTENV" , "ANOTHER_ENV"]) $ do
    it "loads the configuration options to the environment from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] False

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset"

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] False

      lookupEnv "DOTENV" `shouldReturn` Just "preset"

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset"

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] True

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when the .env.example is present" $ do
      let config = Config ["spec/fixtures/.dotenv"] ["spec/fixtures/.dotenv.example"] False

      context "when the needed env vars are missing" $
        it "should fail with an error call" $
          void $ loadFile config `shouldThrow` anyErrorCall

      context "when the needed env vars are not missing" $
        it "should succeed when loading all of the needed env vars" $ do
          setEnv "ANOTHER_ENV" "hello"
          me <- getEnv "USER"
          home <- getEnv "HOME"
          loadFile config `shouldReturn` [("DOTENV","true"),("UNICODE_TEST","Manab\237"),("ENVIRONMENT", home),("PREVIOUS","true"),("ME", me),("ANOTHER_ENV","")]
          lookupEnv "DOTENV" `shouldReturn` Just "true"
          lookupEnv "UNICODE_TEST" `shouldReturn` Just "Manabí"
          lookupEnv "ANOTHER_ENV" `shouldReturn` Just "hello"

  describe "parseFile" $ after_ (unsetEnv "DOTENV") $ do
    it "returns variables from a file without changing the environment" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      liftM head (parseFile "spec/fixtures/.dotenv") `shouldReturn`
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

    it "recognises commands" $ do
      me <- init <$> readCreateProcess (shell "whoami") ""
      liftM (!! 4) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("ME", me)

  describe "onMissingFile" $ after_ (unsetEnv "DOTENV") $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        void $ onMissingFile (loadFile $ Config ["spec/fixtures/.dotenv"] [] True) (return [])
        lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when target file is missing" $
      it "executes supplied handler instead" $
        onMissingFile (True <$ (loadFile $ Config ["spec/fixtures/foo"] [] True)) (return False)
          `shouldReturn` False
