{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.DotenvSpec (main, spec) where

import Configuration.Dotenv.Types (Config(..))
import Configuration.Dotenv
  ( load
  , loadFile
  , loadSafeFile
  , parseFile
  , onMissingFile
  )
import Configuration.Dotenv.Scheme (SchemaError(..))


import Test.Hspec

import System.Process (readCreateProcess, shell)
import System.Exit (ExitCode(..))
import System.Environment (lookupEnv)
import Control.Monad (liftM, void)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif

#if MIN_VERSION_base(4,7,0)
import System.Environment (getEnvironment, setEnv, unsetEnv)
#else
import System.Environment.Compat (getEnvironment, setEnv, unsetEnv)
#endif

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

  describe "loadSafeFile" $ after_ clearEnvs $
    context "given a custom map" $ do
      context "when the envs are written accordingly to the rules in the map" $
        it "should validate accordingly to the rules in the custom map" $
          let hasTwoLetters text = T.length text == 2
              customMap = M.fromList
                [ ("twoLetters", hasTwoLetters)
                , ("bool", const True)
                , ("text", const True)
                , ("integer", const True)
                ]
              schemaFile = "spec/fixtures/.scheme.yml"
              config = Config ["spec/fixtures/.dotenv.safe"] [] False
              expectedEnvs =
                [ ("DOTENV","true")
                , ("OTHERENV","false")
                , ("PORT","8000")
                , ("URL","http://example.com")
                , ("TWO","xD")
                ]
           in do
             envs <- loadSafeFile customMap schemaFile config
             envs `shouldMatchList` expectedEnvs

      context "when the envs are written in an unexpected way" $
        it "should throw an errorCall" $
          let unexpectedFormat text = T.length text == 3
              customMap = M.fromList
                [ ("twoLetters", unexpectedFormat)
                , ("bool", const True)
                , ("text", const True)
                , ("integer", const True)
                ]
              schemaFile = "spec/fixtures/.scheme.yml"
              config = Config ["spec/fixtures/.dotenv.safe"] [] False
           in void $ loadSafeFile customMap schemaFile config
                `shouldThrow` (== ExitFailure 1)


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

  describe "onMissingFile" $ after_ clearEnvs $ do
    context "when target file is present" $
      it "loading works as usual" $ do
        void $ onMissingFile (loadFile $ Config ["spec/fixtures/.dotenv"] [] True) (return [])
        lookupEnv "DOTENV" `shouldReturn` Just "true"

    context "when target file is missing" $
      it "executes supplied handler instead" $
        onMissingFile (True <$ loadFile (Config ["spec/fixtures/foo"] [] True)) (return False)
          `shouldReturn` False

clearEnvs :: IO ()
clearEnvs =
  fmap (fmap fst) getEnvironment >>=  mapM_ unsetEnv

setupEnv :: IO ()
setupEnv = do
  setEnv "ANOTHER_ENV" "hello"
  setEnv "HOME" "/home/me"
