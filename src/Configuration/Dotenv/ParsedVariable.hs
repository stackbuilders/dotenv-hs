{-# LANGUAGE CPP #-}

module Configuration.Dotenv.ParsedVariable (ParsedVariable(..),
                                            VarName,
                                            VarValue(..),
                                            VarContents,
                                            VarFragment(..),
                                            interpolateParsedVariables) where

import Control.Monad (foldM)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Control.Applicative ((<|>))
import System.Environment (lookupEnv)

data ParsedVariable
  = ParsedVariable VarName VarValue deriving (Show, Eq)

type VarName = String

data VarValue
  = Unquoted VarContents
  | SingleQuoted VarContents
  | DoubleQuoted VarContents deriving (Show, Eq)

type VarContents = [VarFragment]

data VarFragment
  = VarInterpolation String
  | VarLiteral String deriving (Show, Eq)

interpolateParsedVariables :: [ParsedVariable] -> IO [(String, String)]
interpolateParsedVariables = fmap reverse . foldM addInterpolated []

addInterpolated :: [(String, String)] -> ParsedVariable -> IO [(String, String)]
addInterpolated previous (ParsedVariable name value) = (: previous) <$> ((,) name <$> interpolate previous value)

interpolate :: [(String, String)] -> VarValue -> IO String
interpolate _        (SingleQuoted contents) = return $ joinContents contents
interpolate previous (DoubleQuoted contents) = interpolateContents previous contents
interpolate previous (Unquoted     contents) = interpolateContents previous contents

interpolateContents :: [(String, String)] -> VarContents -> IO String
interpolateContents previous contents = concat <$> mapM (interpolateFragment previous) contents

interpolateFragment :: [(String, String)] -> VarFragment -> IO String
interpolateFragment _        (VarLiteral       value  ) = return value
interpolateFragment previous (VarInterpolation varname) = fromPreviousOrEnv >>= maybe (return "") return
  where
    fromPreviousOrEnv = (lookup varname previous <|>) <$> lookupEnv varname

joinContents :: VarContents -> String
joinContents = concatMap fragmentToString
  where
    fragmentToString (VarInterpolation value) = value
    fragmentToString (VarLiteral value)       = value
