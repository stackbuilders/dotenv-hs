{-# LANGUAGE CPP #-}

module Configuration.DotenvSpec where

import Control.Monad (void)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif

#if MIN_VERSION_base(4,7,0)
import System.Environment (setEnv, getEnv, lookupEnv, unsetEnv)
import Data.Monoid (mempty)
#else
import System.Environment.Compat (setEnv, getEnv, lookupEnv, unsetEnv)
#endif
import Test.Hspec

import Configuration.Dotenv (loadFile, onMissingFile)
import Configuration.Dotenv.Types

fixturesPath :: FilePath
fixturesPath = "spec/fixtures/"

buildConfig :: [FilePath] -> Bool -> String -> Config
buildConfig dotenvExample allowOverride dotenv =
  Config { configExamplePath = dotenvExample
         , configOverride    = allowOverride
         , configPath        = [ fixturesPath ++ dotenv ]
         }

spec :: Spec
spec = after_ (mapM_ unsetEnv ["DOTENV", "UNICODE_TEST", "ANOTHER_ENV"]) $ do
  describe "loadFile" $ do
    context "when the env variables are defined in the environment" $ do
      context "when config override is set to False" $
        it "reads the env variables from the environment" $ do
          setEnv "DOTENV" "false"
          home <- getEnv "HOME"

          let config = buildConfig mempty False ".dotenv"
          loadFile config
            `shouldReturn`
              [("DOTENV", "false"), ("UNICODE_TEST", "Manabí"), ("ENVIRONMENT", home), ("PREVIOUS", "true")]

      context "when config override is set to True" $
        it "reads the env variables from the dotenv file" $ do
          setEnv "DOTENV" "false"
          home <- getEnv "HOME"

          let config = buildConfig mempty True ".dotenv"
          loadFile config
            `shouldReturn`
              [("DOTENV", "true"), ("UNICODE_TEST", "Manabí"), ("ENVIRONMENT", home), ("PREVIOUS", "true")]

    context "when the env variables are not defined in the environment" $ do
      context "when the variables are defined in the dotenv file" $
        it "reads the env vars from the dotenv file" $ do
          let config = buildConfig mempty False ".dotenv"
          setEnv "DOTENV" "false"
          home <- getEnv "HOME"

          loadFile config
            `shouldReturn`
              [("DOTENV", "false"), ("UNICODE_TEST", "Manabí"), ("ENVIRONMENT", home), ("PREVIOUS", "true")]

      context "when the variables are not defined in the dotenv file" $
        it "fails because of missing keys" $ do
          let config = buildConfig [fixturesPath ++ ".dotenv.example"] False ".incomplete.dotenv"
          loadFile config `shouldThrow` anyErrorCall

  context "when the files are missing or badly formatted" $ do
    it "fails when .env.example is missing" $ do
      let config = buildConfig [fixturesPath ++ ".missing.dotenv.example"] False ".dotenv"
      loadFile config `shouldThrow` anyIOException

    it "fails when .env.example is badly formatted" $ do
      let config = buildConfig [fixturesPath ++ ".bad.dotenv.example"] False ".dotenv"
      loadFile config `shouldThrow` anyErrorCall

  context "when the safety mode is on" $ do
    context "when an env var is missing in the environment or in the dotenv file" $
      it "fails because of missing env vars" $ do
        let config = buildConfig [fixturesPath ++ ".dotenv.example"] False ".dotenv"
        loadFile config `shouldThrow` anyErrorCall

    context "when all the env vars are setted" $
      it "success" $ do
        let config = buildConfig [fixturesPath ++ ".dotenv.example"] False ".dotenv"
        setEnv "ANOTHER_ENV" "SOME_BAR"
        home <- getEnv "HOME"

        loadFile config
          `shouldReturn`
            [("DOTENV", "true"), ("UNICODE_TEST", "Manabí"), ("ENVIRONMENT", home), ("PREVIOUS", "true"), ("ANOTHER_ENV", "SOME_BAR")]

  describe "onMissingFile" $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        let config = buildConfig mempty True ".dotenv"
        void $ onMissingFile (loadFile config) (return [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")])
        lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when target file is missing" $
      it "executes supplied handler instead" $ do
        let config = buildConfig mempty True ".missing.dotenv"
        onMissingFile (True <$ loadFile config) (return False) `shouldReturn` False

