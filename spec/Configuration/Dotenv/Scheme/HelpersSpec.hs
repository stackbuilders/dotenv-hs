module Configuration.Dotenv.Scheme.HelpersSpec where

import Control.Exception.Base (evaluate)
import Test.Hspec

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

spec :: Spec
spec = do
  describe "matchVarWithType" $ do
    context "when the config is empty" $
      context "when there are envs in the dotenv file" $
        it "should fail because it can't match the type" $
          let config = Config []
              env = ("DOTENV", "true")
           in evaluate (matchVarWithType config env) `shouldThrow` anyErrorCall

    context "when the config doesn't have the env" $
      it "should fail because it can't match the type" $
        let envConfs = [EnvConf EnvBool ["NODOTENV"]]
            config = Config envConfs
            env = ("DOTENV", "true")
         in evaluate (matchVarWithType config env) `shouldThrow` anyErrorCall

    context "when the config has the env" $
      it "returns Just (envType, envName)" $
        let envConfs = [EnvConf EnvBool ["DOTENV"]]
            config = Config envConfs
            env = ("DOTENV", "true")
         in matchVarWithType config env `shouldBe` ("true", EnvBool)

  describe "mapMatchVarWithType" $ do
    context "when the envs are defined in the scheme" $
      it "maps the env with the specific content" $
        let envConfs = [ EnvConf EnvBool ["DOTENV"]
                       , EnvConf EnvInteger ["OTHER_ENV"]]
            config = Config envConfs
            env = [("DOTENV", "true"), ("OTHER_ENV", "123")]
         in mapMatchVarWithType config env `shouldBe` [("true", EnvBool), ("123", EnvInteger)]

    context "when there are missing envs in the scheme" $
      it "should not return the env matching the type" $
        let envConfs = [EnvConf EnvBool ["DOTENV"]]
            config = Config envConfs
            env = [("DOTENV", "true"), ("OTHER_ENV", "123")]
         in evaluate (mapMatchVarWithType config env) `shouldNotReturn` [("DOTENV", EnvBool)]

    context "when there are extra envs in the scheme" $
      it "maps the env with the specific content" $
        let envConfs = [ EnvConf EnvBool ["DOTENV"]
                       , EnvConf EnvInteger ["OTHER_ENV"]
                       , EnvConf EnvInteger ["EXTRA_ENV"]]
            config = Config envConfs
            env = [("DOTENV", "true"), ("OTHER_ENV", "123")]
         in mapMatchVarWithType config env `shouldBe` [("true", EnvBool), ("123", EnvInteger)]
