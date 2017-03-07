{-# LANGUAGE CPP #-}

module Configuration.Dotenv.FileSpec where

import Test.Hspec

import Configuration.Dotenv.File

spec :: Spec
spec = do
  describe "parseMaybeFile" $ do
    it "returns Nothing when the file is not found" $
      parseMaybeFile "path/to/nothing" `shouldReturn` Nothing

    it "returns parsed variables from file" $
      parseMaybeFile "spec/fixtures/.dotenv"
        `shouldReturn`
          Just [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")]

  describe "parseFile" $
    it "returns parsed variables from file" $
      parseFile "spec/fixtures/.dotenv"
        `shouldReturn`
          [("DOTENV", "true"), ("UNICODE_TEST", "Manabí")]
