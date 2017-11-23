{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Scheme.Helpers where

import Data.List

import Configuration.Dotenv.Scheme.Types

matchVarWithType
  :: Config           -- ^ List of EnvConf for variables
  -> (String, String) -- ^ (Env Name, Env Value)
  -> (String, EnvType)
matchVarWithType (Config envConfs) (name, value) =
  let criteria EnvConf{..} = name `elem` envNames
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
mapMatchVarWithType config = map (matchVarWithType config)
