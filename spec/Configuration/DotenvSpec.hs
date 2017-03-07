{-# LANGUAGE CPP #-}

module Configuration.DotenvSpec where

import Control.Monad (void)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif

#if MIN_VERSION_base(4,7,0)
import System.Environment (setEnv, lookupEnv, unsetEnv)
#else
import System.Environment.Compat (setEnv, lookupEnv, unsetEnv)
#endif
import Test.Hspec

import Configuration.Dotenv (loadFile, onMissingFile)
import Configuration.Dotenv.Types

buildConfig :: String -> Bool -> String -> Config
buildConfig dotenvExample allowOverride dotenv =
  Config { configExamplePath = "spec/fixtures/" ++ dotenvExample
         , configOverride    = allowOverride
         , configPath        = "spec/fixtures/" ++ dotenv
         }

spec :: Spec
spec = after_ (mapM_ unsetEnv ["DOTENV", "UNICODE_TEST"]) $ do
  describe "loadFile" $ do
    context "when the env variables are defined in the environment" $ do
      context "when config override is set to False" $
        it "reads the env variables from the environment" $ do
          setEnv "DOTENV" "false"

          let config = buildConfig ".dotenv.example" False ".dotenv"
          loadFile config `shouldReturn` [("DOTENV", "false"), ("UNICODE_TEST", "Manabí")]

      context "when config override is set to True" $
        it "reads the env variables from the dotenv file" $ do
          setEnv "DOTENV" "false"

          let config = buildConfig ".dotenv.example" True ".dotenv"
          loadFile config `shouldReturn` [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")]

    context "when the env variables are not defined in the environment" $ do
      context "when the variables are defined in the dotenv file" $
        it "reads the env vars from the dotenv file" $ do
          let config = buildConfig ".dotenv.example" False ".dotenv"
          loadFile config `shouldReturn` [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")]

      context "when the variables are not defined in the dotenv file" $
        it "fails because of missing keys" $ do
          let config = buildConfig ".dotenv.example" False ".incomplete.dotenv"
          loadFile config `shouldThrow` anyIOException

    it "fails when .env.example is missing" $ do
      let config = buildConfig ".missing.dotenv.example" False ".dotenv"
      loadFile config `shouldThrow` anyIOException

    it "fails when .env.example is badly formatted" $ do
      let config = buildConfig ".bad.dotenv.example" False ".dotenv"
      loadFile config `shouldThrow` anyErrorCall

  describe "onMissingFile" $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        let config = buildConfig ".dotenv.example" True ".dotenv"
        void $ onMissingFile (loadFile config) (return [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")])
        lookupEnv "DOTENV" `shouldReturn` Just "true"
    context "when target file is missing" $
      it "executes supplied handler instead" $ do
        let config = buildConfig ".dotenv.example" True ".missing.dotenv"
        onMissingFile (True <$ loadFile config) (return False) `shouldReturn` False

