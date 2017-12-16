{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Scheme.Helpers
  ( genEnvsWithType
  , getRequired
  , joinEnvs
  , matchValueAndType
  , missingDotenvs
  , missingSchemeEnvs
  , sepWithCommas
  , showMissingDotenvs
  , showMissingSchemeEnvs
  )
  where

import Data.List

import Configuration.Dotenv.Scheme.Types

matchValueAndType
  :: [((Env, EnvType), (String, String))]
  -> [(String, EnvType)]
matchValueAndType =
  let getValueAndType ((_, envType), (_, value)) = (value, envType)
   in map getValueAndType

genEnvsWithType :: [EnvConf] -> [(Env, EnvType)]
genEnvsWithType =
  let genEnvWithType EnvConf{..} = (,) <$> envs <*> pure envType
   in concatMap genEnvWithType

joinEnvs
  :: [(Env, EnvType)]
  -> [(String, String)]
  -> [((Env, EnvType), (String, String))]
joinEnvs =
  let sameName ((Env{..},_), (name,_)) = envName == name
   in joinBy sameName

joinBy :: ((a,b) -> Bool) -> [a] -> [b] -> [(a,b)]
joinBy p xs ys =
  let cartesianProduct = (,) <$> xs <*> ys
   in filter p cartesianProduct

missingDotenvs
  :: [(Env, EnvType)]
  -> [((Env, EnvType), (String, String))]
  -> [(Env, EnvType)]
missingDotenvs =
  let sameName (envOne,_) (envTwo,_) = envName envOne == envName envTwo
   in missingLeft sameName

missingLeft :: (a -> a -> Bool) -> [a] -> [(a,b)] -> [a]
missingLeft p xs xys =
  let getAllLeft = map fst xys
   in deleteFirstsBy p xs getAllLeft

missingSchemeEnvs
  :: [(String, String)]
  -> [((Env, EnvType), (String, String))]
  -> [(String, String)]
missingSchemeEnvs =
  let sameName (nameOne,_) (nameTwo,_) = nameOne == nameTwo
   in missingRight sameName

missingRight :: (b -> b -> Bool) -> [b] -> [(a,b)] -> [b]
missingRight p ys xys =
  let getAllRight = map snd xys
   in deleteFirstsBy p ys getAllRight

getRequired :: [(Env,EnvType)] -> [(Env,EnvType)]
getRequired = filter (required . fst)

sepWithCommas :: [String] -> String
sepWithCommas = concat . intersperse ", "

showMissingDotenvs :: [(Env, EnvType)] -> String
showMissingDotenvs = sepWithCommas . map (envName . fst)

showMissingSchemeEnvs :: [(String, String)] -> String
showMissingSchemeEnvs = sepWithCommas . map fst
