{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Scheme.Helpers
  ( findEnvByName
  , mapMatchVarWithType
  )
  where

import Data.List
import Data.Maybe

import Configuration.Dotenv.Scheme.Types

mapMatchVarWithType
  :: Config           -- ^ List of EnvConf for variables
  -> [(String, String)] -- ^ (Env Name, Env Value)
  -> [(String, EnvType)]
mapMatchVarWithType (Config envConfs) envvars =
  let envsWithType = genEnvsWithType envConfs
      valuesAndTypes = fmap (findEnvByName envvars) envsWithType
   in catMaybes valuesAndTypes

findEnvByName
  :: [(String, String)] -- ^ [(Env Name, Env Value)]
  -> (Env, EnvType)
  -> Maybe (String, EnvType)
findEnvByName envs (Env{..}, envType) =
  let criteria (name, _) = name == envName
      maybeEnv = find criteria envs
   in case maybeEnv of
        Just (_, value) -> Just (value, envType)
        Nothing ->
          if required
             then error $ "The env: " ++ envName ++ " should be defined in the dotenvs."
             else Nothing

genEnvsWithType :: [EnvConf] -> [(Env, EnvType)]
genEnvsWithType =
  let genEnvWithType EnvConf{..} = (,) <$> envs <*> pure envType
   in concatMap genEnvWithType
