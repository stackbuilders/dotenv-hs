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
    | EnvText
    deriving (Show, Eq, Ord)

instance FromJSON EnvType where
  parseJSON (String "integer") = pure EnvInteger
  parseJSON (String "bool") = pure EnvBool
  parseJSON (String "text") = pure EnvText
  parseJSON (String x) = fail ("Don't know how to parse that kind of type: " ++ show x)
  parseJSON x = fail ("Not an object: " ++ show x)

data Env =
  Env
    { envName  :: String
    , envType  :: EnvType
    , required :: Bool
    } deriving (Show, Eq, Ord)

instance FromJSON Env where
  parseJSON (Object m) =
    Env
      <$> m .: "name"
      <*> m .: "type"
      <*> m .:? "required" .!= False
  parseJSON x = fail ("Not an object: " ++ show x)
