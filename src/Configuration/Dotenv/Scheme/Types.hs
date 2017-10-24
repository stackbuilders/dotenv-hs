{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Types where

import Control.Applicative
import Data.Yaml

data EnvType =
  EnvInteger
    | EnvBool
    | EnvText deriving (Eq)

data EnvConf =
  EnvConf
    { envType :: EnvType
    , envName :: String
    } deriving (Show, Eq)

instance Show EnvType where
  show EnvInteger = "integer"
  show EnvBool    = "bool"
  show EnvText    = "text"

instance FromJSON EnvType where
  parseJSON (String "integer") = pure EnvInteger
  parseJSON (String "bool") = pure EnvBool
  parseJSON (String "text") = pure EnvText
  parseJSON (String x) = fail ("Don't know how to parse that kind of type: " ++ show x)
  parseJSON x = fail ("Not an object: " ++ show x)

instance FromJSON EnvConf where
  parseJSON (Object m) =
    EnvConf
      <$> m .: "type"
      <*> m .: "name"
  parseJSON x = fail ("not an object: " ++ show x)

newtype Config = Config [EnvConf] deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object m) =
    Config <$> m .: "envs"
  parseJSON x = fail ("not an object: " ++ show x)
