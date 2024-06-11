module Configuration.Dotenv.TextSpec (main, spec) where

import           Configuration.Dotenv.Environment (lookupEnv, unsetEnv)
import           Configuration.Dotenv.Internal    (parseFile)

import           Test.Hspec

import           Control.Monad                    (liftM)
import qualified Data.Text                        as T

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parseFile" $ after_ (unsetEnv "DOTENV") $ do
    it "returns variables from a file without changing the environment" $ do
      lookupEnv "DOTENV" `shouldReturn` Nothing

      liftM head (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "DOTENV", T.pack "true")

      lookupEnv "DOTENV" `shouldReturn` Nothing

    it "recognizes unicode characters" $
      liftM (!! 1) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "UNICODE_TEST", T.pack "Manabí")

    it "handles newline characters correctly" $ do
      liftM (!! 6) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "NEWLINE_TEST", T.pack "Hello\nWorld")

    it "handles manual line breaks correctly" $ do
      liftM (!! 7) (parseFile "spec/fixtures/.dotenv") `shouldReturn`
        (T.pack "MULTILINE_TEST", T.pack "Roses are red\nViolets are blue\nCode is my art\nAnd bugs are my glue")
