{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Configuration.Dotenv.Scheme.Parser
  ( defaultValidatorMap
  , parseEnvsWithScheme
  , typeValidator
  )
  where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), pure)
import Data.Functor ((<$>))
#endif

import Data.Either
import Data.Maybe (isJust)

import qualified Data.Map.Lazy as ML

import Text.Read (readMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Configuration.Dotenv.Scheme.Types


-- |
--
parseEnvsWithScheme
  :: ValidatorMap -- ^ MapFormat validations
  -> [(String, EnvType)] -- ^ Value and Type
  -> Either [String] ()
parseEnvsWithScheme validatorMap valuesAndTypes =
  let parsedEithers = parseEnvs <$> valuesAndTypes
      parseEnvWith = typeValidator validatorMap
      parseEnvs (val, type') = val `parseEnvWith` type'
    in if all isRight parsedEithers
          then Right ()
          else Left (lefts parsedEithers)

-- |
--
typeValidator
  :: ValidatorMap -- ^ MapFormat validations
  -> String -- ^ Value of the env variable
  -> EnvType -- ^ Type that the env variable should have
  -> Either String ()
typeValidator validatorMap envVal (EnvType type_) =
  let errorMsg = "Couldn't parse " ++ envVal ++ " as " ++ T.unpack type_
   in case ML.lookup type_ validatorMap of
        Nothing -> Left errorMsg
        Just validator ->
          if validator (T.pack envVal)
             then Right ()
             else Left errorMsg

-- |
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
