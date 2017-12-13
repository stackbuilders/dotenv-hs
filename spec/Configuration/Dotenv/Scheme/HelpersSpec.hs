module Configuration.Dotenv.Scheme.HelpersSpec where

import Control.Exception.Base (evaluate)
import Test.Hspec

import Configuration.Dotenv.Scheme.Helpers
import Configuration.Dotenv.Scheme.Types

spec :: Spec
spec = do
  describe "findEnvByName" $
    let envvars = [("FOO", "true"), ("BAR", "123")]
     in do
       context "when the env is in the scheme and in the dotenv" $
         it "should return Just (envValue, envType)" $
           let envWithType = (Env "FOO" True, EnvBool)
            in findEnvByName envvars envWithType `shouldBe` Just ("true", EnvBool)

       context "when the env is in the scheme, but not in the dotenv" $ do
         context "when the env is required" $
           it "should throw and error message" $
             let envWithTypeReq = (Env "BAZ" True, EnvBool)
                 msg = "The env: BAZ should be defined in the dotenvs."
              in evaluate (findEnvByName envvars envWithTypeReq) `shouldThrow` errorCall msg

         context "when the env is not required" $
           it "should return Nothing" $
             let envWithTypeNotReq = (Env "BAZ" False, EnvBool)
              in findEnvByName envvars envWithTypeNotReq `shouldBe` Nothing

  describe "mapMatchVarWithType" $ do
    context "when the envs are defined in the scheme" $
      it "maps the env with the specific content" $
        let envConfs = [ EnvConf EnvBool [Env "DOTENV" True]
                       , EnvConf EnvInteger [Env "OTHER_ENV" True]
                       ]
            config = Config envConfs
            env = [("DOTENV", "true"), ("OTHER_ENV", "123")]
         in mapMatchVarWithType config env `shouldBe` [("true", EnvBool), ("123", EnvInteger)]

    context "when there are missing envs in the scheme" $
      it "should not return the env matching the type" $
        let envConfs = [EnvConf EnvBool [Env "DOTENV" True]]
            config = Config envConfs
            env = [("DOTENV", "true"), ("OTHER_ENV", "123")]
         in evaluate (mapMatchVarWithType config env) `shouldNotReturn` [("DOTENV", EnvBool)]

    context "when there are envs required and not required in the dotenv" $
      let envConfs =
            [ EnvConf EnvBool [Env "FOO" True]
            , EnvConf EnvInteger [Env "BAR" False]
            ]
         in do
            context "when the dotenv has both kind of envs" $
              it "maps the env with the specific type" $
                let config = Config envConfs
                    env = [("FOO", "true"), ("BAR", "123")]
                in mapMatchVarWithType config env `shouldBe` [("true", EnvBool), ("123", EnvInteger)]

            context "when the dotenv has only the required envs" $
              it "should map the required envs with the specific type" $
                let config = Config envConfs
                    env = [("FOO", "true")]
                in mapMatchVarWithType config env `shouldBe` [("true", EnvBool)]

            context "when the dotenv has only the not required envs" $
              it "should throw a message error" $
                let config = Config envConfs
                    env = [("BAR", "123")]
                    msg = "The env: FOO should be defined in the dotenvs."
                 in evaluate (mapMatchVarWithType config env) `shouldThrow` errorCall msg
