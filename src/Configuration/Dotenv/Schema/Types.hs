-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2018 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for 'loadSafeFile' (e. g., 'ValidatorMap')

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration.Dotenv.Schema.Types where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif

import Data.Maybe (isJust)

import Data.Map.Lazy (Map)

import Data.Yaml

import qualified Data.Map.Lazy as ML

import Data.Text (Text)
import qualified Data.Text as T

import Text.Read (readMaybe)


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

-- | Parameters:
--
-- - __Key:__ Name of the /format/ to check.
--
-- - __Value:__ Function to check if some text meets the condition.
--
type ValidatorMap = Map Text (Text -> Bool)


-- | Default configuration for 'loadSafeFile'. It currently checks:
-- @bool@, @integer@, and @text@.
--
defaultValidatorMap :: ValidatorMap
defaultValidatorMap =
  let booleanValidator :: Text -> Bool
      booleanValidator text = isJust (readMaybe (T.unpack text) :: Maybe Bool)
      integerValidator :: Text -> Bool
      integerValidator text = isJust (readMaybe (T.unpack text) :: Maybe Integer)
      textValidator :: Text -> Bool
      textValidator = const True
   in ML.fromList
        [ ("bool", booleanValidator)
        , ("integer", integerValidator)
        , ("text", textValidator)
        ]
