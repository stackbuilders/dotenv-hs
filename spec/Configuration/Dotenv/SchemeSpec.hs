{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.SchemeSpec (spec) where

import Configuration.Dotenv.Scheme
import Configuration.Dotenv.Scheme.Types
import Control.Exception (evaluate)

import Test.Hspec

-- |
--
spec :: Spec
spec = do
  describe "checkScheme" $ do
    context "when the env configs are unique" $
      it "should succeed the check" $
        let schemeEnvs =
              [ Env "FOO" (EnvType "bool") True
              , Env "BAR" (EnvType "integer") True
              ]
         in checkScheme schemeEnvs `shouldBe` schemeEnvs

    context "when there are duplicated env configs" $
      it "should fail the check" $
        let duplicatedEnvs =
              [ Env "FOO" (EnvType "bool") True
              , Env "FOO" (EnvType "integer") True
              ]
            schemeEnvs = Env "BAR" (EnvType "integer") True : duplicatedEnvs
         in evaluate (checkScheme schemeEnvs)
              `shouldThrow` (\(DuplicatedEnvs _) -> True)

  describe "checkConfig" $
    context "when the envs have the correct type" $ do
      context "when the envs in the dotenvs are defined in the scheme" $ do
        context "when the required envs are defined" $
          it "should succeed the type check" $
            let schemeEnvs =
                    [ Env "FOO" (EnvType "bool") True
                    , Env "BAR" (EnvType "integer") False
                    ]
                dotenvs = [("FOO","True"), ("BAR","123")]
             in checkConfig defaultValidatorMap dotenvs schemeEnvs `shouldReturn` ()

        context "when the not required envs are missing" $
          it "should succeed the type check" $
            let schemeEnvs =
                    [ Env "FOO" (EnvType "bool") True
                    , Env "BAR" (EnvType "integer") False
                    ]
                dotenvs = [("FOO","True")]
             in checkConfig defaultValidatorMap dotenvs schemeEnvs `shouldReturn` ()

        context "when the required envs are missing" $
          it "should fail before the type check" $
            let missingEnv = Env "FOO" (EnvType "bool") True
                schemeEnvs =
                    [ Env "BAR" (EnvType "integer") False
                    , missingEnv
                    ]
                dotenvs = [("BAR","123")]
              in checkConfig defaultValidatorMap dotenvs schemeEnvs
                   `shouldThrow` (\(MissingEnvsInDotenvs _) -> True)

      context "when there are missing dotenvs in the scheme" $
        it "should fail before type checking" $
          let schemeEnvs =
                  [ Env "FOO" (EnvType "bool") True
                  , Env "BAR" (EnvType "integer") False
                  ]
              missingEnv = ("BAZ","text")
              dotenvs = [("FOO","True"), ("BAR","123"), missingEnv]
           in checkConfig defaultValidatorMap dotenvs schemeEnvs
                `shouldThrow` (\(MissingEnvsInSchema _) -> True)

      context "when there are missing scheme envs in the dotenv vars" $
        it "should fail before type checking" $
          let schemeEnvs =
                  [ Env "FOO" (EnvType "bool") True
                  , Env "BAR" (EnvType "integer") False
                  , missingEnv
                  ]
              missingEnv = Env "BAZ" (EnvType "text") True
              dotenvs = [("FOO","True"), ("BAR","123")]
           in checkConfig defaultValidatorMap dotenvs schemeEnvs
                `shouldThrow` (\(MissingEnvsInDotenvs _) -> True)
