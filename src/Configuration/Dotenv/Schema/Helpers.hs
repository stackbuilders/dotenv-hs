-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2018 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for loadSafeFile

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration.Dotenv.Schema.Helpers
  ( joinEnvs
  , matchValueAndType
  , missingDotenvs
  , missingSchemaEnvs
  , sepWithCommas
  , showMissingDotenvs
  , showMissingSchemaEnvs
  )
  where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
#endif
import Data.List

import Configuration.Dotenv.Schema.Types

matchValueAndType
  :: [(Env, (String, String))]
  -> [(String, EnvType)]
matchValueAndType =
  let getValueAndType (Env{..}, (_, value)) = (value, envType)
   in map getValueAndType

joinEnvs
  :: [Env]
  -> [(String, String)]
  -> [(Env, (String, String))]
joinEnvs =
  let sameName (Env{..}, (name,_)) = envName == name
   in joinBy sameName

joinBy :: ((a,b) -> Bool) -> [a] -> [b] -> [(a,b)]
joinBy p xs ys =
  let cartesianProduct = (,) <$> xs <*> ys
   in filter p cartesianProduct

missingDotenvs
  :: [Env]
  -> [(Env, (String, String))]
  -> [Env]
missingDotenvs =
  let sameName envOne envTwo = envName envOne == envName envTwo
   in missingLeft sameName

missingLeft :: (a -> a -> Bool) -> [a] -> [(a,b)] -> [a]
missingLeft p xs xys =
  let getAllLeft = map fst xys
   in deleteFirstsBy p xs getAllLeft

missingSchemaEnvs
  :: [(String, String)]
  -> [(Env, (String, String))]
  -> [(String, String)]
missingSchemaEnvs =
  let sameName (nameOne,_) (nameTwo,_) = nameOne == nameTwo
   in missingRight sameName

missingRight :: (b -> b -> Bool) -> [b] -> [(a,b)] -> [b]
missingRight p ys xys =
  let getAllRight = map snd xys
   in deleteFirstsBy p ys getAllRight

sepWithCommas :: [String] -> String
sepWithCommas = intercalate ", "

showMissingDotenvs :: [Env] -> String
showMissingDotenvs = sepWithCommas . map envName

showMissingSchemaEnvs :: [(String, String)] -> String
showMissingSchemaEnvs = sepWithCommas . map fst
