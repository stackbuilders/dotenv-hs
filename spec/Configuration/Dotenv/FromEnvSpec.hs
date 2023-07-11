{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Configuration.Dotenv.FromEnvSpec (main, spec) where

import           GHC.Generics
import           Test.Hspec                       (Spec, after_, describe,
                                                   hspec, it, shouldBe,
                                                   shouldSatisfy)

import           Configuration.Dotenv.Environment (getEnvironment, setEnv,
                                                   unsetEnv)
import           Configuration.Dotenv.FromEnv
import           Data.Maybe                       (fromJust, isJust)

data Config = Config
  { dbURL  :: !String
  , apiKey :: !String
  }
  deriving (Eq, Show, Generic, FromEnv)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromEnv" $ after_ clearEnvs $ do
    it "returns Nothing when the necessary environment variables are not set" $ do
      config <- fromEnv_ @Config
      config `shouldBe` Nothing

    it "returns Nothing when only one environment variable is missing" $ do
      setEnv "dbURL" "hello"
      config <- fromEnv_ @Config
      config `shouldBe` Nothing

    it "returns the configuration object when all necessary variables are set" $ do
      setEnv "dbURL" "hello"
      setEnv "apiKey" "world"
      config <- fromEnv_
      config `shouldSatisfy` isJust
      fromJust config `shouldBe` Config "hello" "world"

    it "returns the configuration object when using different names for the environment variables" $ do
      setEnv "DB_URL" "hello"
      setEnv "API_KEY" "world"
      let converter "dbURL"  = Just "DB_URL"
          converter "apiKey" = Just "API_KEY"
          converter _        = Nothing
      config <- fromEnv converter
      config `shouldSatisfy` isJust
      fromJust config `shouldBe` Config "hello" "world"


clearEnvs :: IO ()
clearEnvs = getEnvironment >>= mapM_ unsetEnv . fmap fst
