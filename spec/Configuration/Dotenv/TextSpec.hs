{-# LANGUAGE CPP #-}

module Configuration.Dotenv.TextSpec (main, spec) where

import Configuration.Dotenv.Text (parseFile)

import Test.Hspec

import System.Environment (lookupEnv)
import qualified Data.Text as T

#if MIN_VERSION_base(4,7,0)
import System.Environment (unsetEnv)
#else
import System.Environment.Compat (unsetEnv)
#endif

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parseFile" $ after_ (unsetEnv "DOTENV") $ do
    it "returns variables from a file without changing the environment" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      fmap head (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "DOTENV", T.pack "true")

      lookupEnv "DOTENV" `shouldReturn` Nothing

    it "recognizes unicode characters" $
      fmap (!! 1) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "UNICODE_TEST", T.pack "Manabí")
