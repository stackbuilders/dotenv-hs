{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Scheme.Types where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif

import Data.Map.Lazy (Map)

import Data.Yaml

import Data.Text (Text)

-- |
--
newtype EnvType = EnvType Text
    deriving (Show, Eq, Ord)

-- |
--
instance FromJSON EnvType where
  parseJSON (String value) = pure (EnvType value)
  parseJSON anyOther = fail ("Not an object: " ++ show anyOther)

-- |
--
data Env =
  Env
    { envName  :: String
    , envType  :: EnvType
    , required :: Bool
    } deriving (Show, Eq, Ord)

-- |
--
instance FromJSON Env where
  parseJSON (Object m) =
    Env
      <$> m .: "name"
      <*> m .: "type"
      <*> m .:? "required" .!= False
  parseJSON x = fail ("Not an object: " ++ show x)

-- | Key: Name of the "format"
--   Value: Function to check if some text has an specific value
--
type ValidatorMap = Map Text (Text -> Bool)
