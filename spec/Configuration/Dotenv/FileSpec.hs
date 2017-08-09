{-# LANGUAGE CPP #-}

module Configuration.Dotenv.FileSpec where

import Test.Hspec
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import System.Environment (lookupEnv)

import Configuration.Dotenv.File

spec :: Spec
spec = 
  describe "parseFile" $
    it "returns parsed variables from file" $ do
      home <- fromMaybe "" <$> lookupEnv "HOME"
      parseFile "spec/fixtures/.dotenv"
        `shouldReturn`
          [("DOTENV", "true"), ("UNICODE_TEST", "Manabí"), ("ENVIRONMENT", home), ("PREVIOUS", "true")]
