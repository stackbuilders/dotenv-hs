module Configuration.Dotenv.Scheme.HelpersSpec where

import Test.Hspec

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

spec :: Spec
spec = do
  describe "matchVarWithType" $ do
    context "when the config is empty" $
      it "returns Nothing" $
        let config = Config []
            env = ("DOTENV", "true")
         in matchVarWithType config env `shouldBe` Nothing

    context "when the config doesn't have the env" $
      it "returns Nothing" $
        let envConfs = [EnvConf EnvBool "NODOTENV"]
            config = Config envConfs
            env = ("DOTENV", "true")
         in matchVarWithType config env `shouldBe` Nothing

    context "when the config has the env" $
      it "returns Just (envType, envName)" $
        let envConfs = [EnvConf EnvBool "DOTENV", EnvConf EnvInteger "OTHER_ENV"]
            config = Config envConfs
            env = ("DOTENV", "true")
         in matchVarWithType config env `shouldBe` Just ("true", EnvBool)

  describe "mapMatchVarWithType" $
    it "maps the env with the specific content" $
      let envConfs = [EnvConf EnvBool "DOTENV", EnvConf EnvInteger "OTHER_ENV"]
          config = Config envConfs
          env = [("DOTENV", "true"), ("OTHER_ENV", "123"), ("TEST", "test")]
       in mapMatchVarWithType config env `shouldBe` [("true", EnvBool), ("123", EnvInteger)]
