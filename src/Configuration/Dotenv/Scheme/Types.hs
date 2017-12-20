{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Types where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif
import Data.Yaml

data EnvType =
  EnvInteger
    | EnvBool
    | EnvText deriving (Show, Eq)

instance FromJSON EnvType where
  parseJSON (String "integer") = pure EnvInteger
  parseJSON (String "bool") = pure EnvBool
  parseJSON (String "text") = pure EnvText
  parseJSON (String x) = fail ("Don't know how to parse that kind of type: " ++ show x)
  parseJSON x = fail ("Not an object: " ++ show x)

newtype Config = Config [EnvConf] deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = fmap Config . parseJSONList

data EnvConf =
  EnvConf
    { envType  :: EnvType
    , envs     :: [Env]
    } deriving (Show, Eq)

instance FromJSON EnvConf where
  parseJSON (Object m) =
    EnvConf
      <$> m .: "type"
      <*> m .: "envs"
  parseJSON x = fail ("Not an object: " ++ show x)

data Env =
  Env
    { envName  :: String
    , required :: Bool
    } deriving (Show, Eq)

instance FromJSON Env where
  parseJSON (Object m) =
    Env
      <$> m .: "name"
      <*> m .: "required"
  parseJSON x = fail ("Not an object: " ++ show x)
