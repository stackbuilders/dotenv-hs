{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Scheme.Helpers where

import Data.List

import Configuration.Dotenv.Scheme.Types

matchVarWithType
  :: Config           -- ^ List of EnvConf for variables
  -> (String, String) -- ^ (Env Name, Env Value)
  -> (String, EnvType)
matchVarWithType (Config envConfs) (name, value) =
  let criteria EnvConf{..} = name `elem` (map envName envs)
      maybeEnvConf = find criteria envConfs
      pairEnvWithConf EnvConf{..} = (value, envType)
   in
   case maybeEnvConf of
     Just envConv -> pairEnvWithConf envConv
     _            -> error $ "The env " ++ name ++ " must be defined in the scheme file."

mapMatchVarWithType
  :: Config           -- ^ List of EnvConf for variables
  -> [(String, String)] -- ^ (Env Name, Env Value)
  -> [(String, EnvType)]
mapMatchVarWithType config@(Config envConfs) envvars =
  let filterRequiredEnvs EnvConf{..} = filter required envs
      requiredEnvs = concatMap filterRequiredEnvs envConfs
      inRequired (name, _) = name `elem` (map envName requiredEnvs)
      filteredEnvs = filter inRequired envvars
   in map (matchVarWithType config) filteredEnvs
