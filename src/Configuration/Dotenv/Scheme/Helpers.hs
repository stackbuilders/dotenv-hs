{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Scheme.Helpers where

import Data.List
import Data.Maybe

import Configuration.Dotenv.Scheme.Types

matchVarWithType
  :: Config           -- ^ List of EnvConf for variables
  -> (String, String) -- ^ (Env Name, Env Value)
  -> Maybe (String, EnvType)
matchVarWithType (Config envConfs) (name, value) =
  let criteria EnvConf{..} = envName == name
      maybeEnvConf = find criteria envConfs
      pairEnvWithConf EnvConf{..} = (value, envType)
   in fmap pairEnvWithConf maybeEnvConf

mapMatchEnvWithConf
  :: Config           -- ^ List of EnvConf for variables
  -> [(String, String)] -- ^ (Env Name, Env Value)
  -> [(String, EnvType)]
mapMatchEnvWithConf config =
  mapMaybe (matchVarWithType config)
