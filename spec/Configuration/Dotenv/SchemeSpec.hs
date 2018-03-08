module Configuration.Dotenv.SchemeSpec (spec) where

import Configuration.Dotenv.Scheme
import Configuration.Dotenv.Scheme.Types
import Control.Exception (evaluate)

import Test.Hspec

spec :: Spec
spec = do
  describe "checkScheme" $ do
    context "when the env configs are unique" $
      it "should succeed the check" $
        let schemeEnvs =
              [ Env "FOO" EnvBool True
              , Env "BAR" EnvInteger True
              ]
         in checkScheme schemeEnvs `shouldBe` schemeEnvs

    context "when there are duplicated env configs" $
      it "should fail the check" $
        let schemeEnvs =
              [ Env "FOO" EnvBool True
              , Env "BAR" EnvInteger True
              , Env "FOO" EnvInteger True
              ]
            msg = "Duplicated env variable configuration in schema: FOO"
         in evaluate (checkScheme schemeEnvs) `shouldThrow` errorCall msg

  describe "checkConfig" $
    context "when the envs have the correct type" $ do
      context "when the envs in the dotenvs are defined in the scheme" $ do
        context "when the required envs are defined" $
          it "should succeed the type check" $
            let schemeEnvs =
                    [ Env "FOO" EnvBool True
                    , Env "BAR" EnvInteger False
                    ]
                dotenvs = [("FOO","true"), ("BAR","123")]
             in checkConfig dotenvs schemeEnvs `shouldReturn` ()

        context "when the not required envs are missing" $
          it "should succeed the type check" $
            let schemeEnvs =
                    [ Env "FOO" EnvBool True
                    , Env "BAR" EnvInteger False
                    ]
                dotenvs = [("FOO","true")]
             in checkConfig dotenvs schemeEnvs `shouldReturn` ()

        context "when the required envs are missing" $
          it "should fail before the type check" $
            let schemeEnvs =
                    [ Env "FOO" EnvBool True
                    , Env "BAR" EnvInteger False
                    ]
                dotenvs = [("BAR","123")]
                msg = "The following envs: FOO must be in the dotenvs"
              in checkConfig dotenvs schemeEnvs `shouldThrow` errorCall msg

      context "when there are missing dotenvs in the scheme" $
        it "should fail before type checking" $
          let schemeEnvs =
                  [ Env "FOO" EnvBool True
                  , Env "BAR" EnvInteger False
                  ]
              dotenvs = [("FOO","true"), ("BAR","123"), ("BAZ","text")]
              msg = "The following envs: BAZ must be in your scheme.yml"
           in checkConfig dotenvs schemeEnvs `shouldThrow` errorCall msg

      context "when there are missing scheme envs in the dotenv vars" $
        it "should fail before type checking" $
          let schemeEnvs =
                  [ Env "FOO" EnvBool True
                  , Env "BAZ" EnvText True
                  , Env "BAR" EnvInteger False
                  ]
              dotenvs = [("FOO","true"), ("BAR","123")]
              msg = "The following envs: BAZ must be in the dotenvs"
           in checkConfig dotenvs schemeEnvs `shouldThrow` errorCall msg
