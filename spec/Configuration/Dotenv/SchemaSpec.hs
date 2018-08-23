{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.SchemaSpec (spec) where

import Configuration.Dotenv.Schema
import Configuration.Dotenv.Schema.Types
import Control.Exception (evaluate)

import Test.Hspec

-- |
--
spec :: Spec
spec = do
  describe "checkSchema" $ do
    context "when the env configs are unique" $
      it "should succeed the check" $
        let schemaEnvs =
              [ Env "FOO" (EnvType "bool") True
              , Env "BAR" (EnvType "integer") True
              ]
         in checkSchema schemaEnvs `shouldBe` schemaEnvs

    context "when there are duplicated env configs" $
      it "should fail the check" $
        let schemaEnvs =
              [ Env "FOO" (EnvType "bool") True
              , Env "BAR" (EnvType "integer") True
              , Env "FOO" (EnvType "integer") True
              ]
            msg = "Duplicated env variable configuration in schema: FOO"
         in evaluate (checkSchema schemaEnvs) `shouldThrow` errorCall msg

  describe "checkConfig" $
    context "when the envs have the correct type" $ do
      context "when the envs in the dotenvs are defined in the schema" $ do
        context "when the required envs are defined" $
          it "should succeed the type check" $
            let schemaEnvs =
                    [ Env "FOO" (EnvType "bool") True
                    , Env "BAR" (EnvType "integer") False
                    ]
                dotenvs = [("FOO","True"), ("BAR","123")]
             in checkConfig defaultValidatorMap dotenvs schemaEnvs `shouldReturn` ()

        context "when the not required envs are missing" $
          it "should succeed the type check" $
            let schemaEnvs =
                    [ Env "FOO" (EnvType "bool") True
                    , Env "BAR" (EnvType "integer") False
                    ]
                dotenvs = [("FOO","True")]
             in checkConfig defaultValidatorMap dotenvs schemaEnvs `shouldReturn` ()

        context "when the required envs are missing" $
          it "should fail before the type check" $
            let schemaEnvs =
                    [ Env "FOO" (EnvType "bool") True
                    , Env "BAR" (EnvType "integer") False
                    ]
                dotenvs = [("BAR","123")]
                msg = "The following envs: FOO must be in the dotenvs"
              in checkConfig defaultValidatorMap dotenvs schemaEnvs `shouldThrow` errorCall msg

      context "when there are missing dotenvs in the schema" $
        it "should fail before type checking" $
          let schemaEnvs =
                  [ Env "FOO" (EnvType "bool") True
                  , Env "BAR" (EnvType "integer") False
                  ]
              dotenvs = [("FOO","True"), ("BAR","123"), ("BAZ","text")]
              msg = "The following envs: BAZ must be in your schema.yml"
           in checkConfig defaultValidatorMap dotenvs schemaEnvs `shouldThrow` errorCall msg

      context "when there are missing schema envs in the dotenv vars" $
        it "should fail before type checking" $
          let schemaEnvs =
                  [ Env "FOO" (EnvType "bool") True
                  , Env "BAZ" (EnvType "text") True
                  , Env "BAR" (EnvType "integer") False
                  ]
              dotenvs = [("FOO","True"), ("BAR","123")]
              msg = "The following envs: BAZ must be in the dotenvs"
           in checkConfig defaultValidatorMap dotenvs schemaEnvs `shouldThrow` errorCall msg
