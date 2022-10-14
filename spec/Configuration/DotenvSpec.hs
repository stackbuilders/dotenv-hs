{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.DotenvSpec (main, spec) where

import           Configuration.Dotenv             (load, loadFile,
                                                   onMissingFile, parseFile)
import           Configuration.Dotenv.Environment (getEnvironment, lookupEnv,
                                                   setEnv, unsetEnv)
import           Configuration.Dotenv.Types       (Config (..))


import           Test.Hspec

import           Control.Monad                    (liftM, void)
import           Data.Maybe                       (fromMaybe)
import           System.Process                   (readCreateProcess, shell)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "load" $ after_ clearEnvs $ do
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

  describe "loadFile" $ before_ setupEnv $ after_ clearEnvs $  do
    it "loads the configuration options to the environment from a file" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] False False True

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    it "respects predefined settings when overload is false" $ do
      setEnv "DOTENV" "preset"

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] False False True

      lookupEnv "DOTENV" `shouldReturn` Just "preset"

    it "overrides predefined settings when overload is true" $ do
      setEnv "DOTENV" "preset"

      void $ loadFile $ Config ["spec/fixtures/.dotenv"] [] True False True

      lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when the .env.example is present" $ do
      let config = Config ["spec/fixtures/.dotenv"] ["spec/fixtures/.dotenv.example"] False False True

      context "when the needed env vars are missing" $
        it "should fail with an error call" $ do
          unsetEnv "ANOTHER_ENV"
          void $ loadFile config `shouldThrow` anyErrorCall

      context "when the needed env vars are not missing" $
        it "should succeed when loading all of the needed env vars" $ do
          -- Load extra information
          me <- init <$> readCreateProcess (shell "whoami") ""
          home <- fromMaybe "" <$> lookupEnv "HOME"

          -- Load envs
          void $ loadFile config

          -- Check existing envs
          lookupEnv "ENVIRONMENT" `shouldReturn` Just home
          lookupEnv "ME" `shouldReturn` Just me
          lookupEnv "DOTENV" `shouldReturn` Just "true"
          lookupEnv "UNICODE_TEST" `shouldReturn` Just "Manabí"
          lookupEnv "ANOTHER_ENV" `shouldReturn` Just "hello"

  describe "parseFile" $ after_ clearEnvs $ do
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

#if MIN_VERSION_base(4,11,0)
    it "recognizes blank variable" $
      liftM (!! 5) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        ("BLANK", "")
#endif

  describe "onMissingFile" $ after_ clearEnvs $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        void $ onMissingFile (loadFile $ Config ["spec/fixtures/.dotenv"] [] True False True) (return [])
        lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when target file is missing" $
      it "executes supplied handler instead" $
        onMissingFile (True <$ loadFile (Config ["spec/fixtures/foo"] [] True False True)) (return False)
          `shouldReturn` False

  describe "onDuplicatedKeys" $ after_ clearEnvs $ do
    context "when target file has duplicated key" $
      it "throws an error" $
        loadFile (Config ["spec/fixtures/.dotenv", "spec/fixtures/.dotenv"] [] True False False)
          `shouldThrow` anyIOException


clearEnvs :: IO ()
clearEnvs =
  getEnvironment >>= mapM_ unsetEnv . fmap fst

setupEnv :: IO ()
setupEnv = do
  setEnv "ANOTHER_ENV" "hello"
  setEnv "HOME" "/home/me"
