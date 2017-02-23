module Configuration.Dotenv.ParsedVariable (ParsedVariable(..),
                                            VName,
                                            VValue(..),
                                            VContents,
                                            VFragment(..),
                                            interpolateParsedVariables) where

import Control.Monad (foldM)
import Control.Applicative ((<|>))
import System.Environment (lookupEnv)

data ParsedVariable
  = ParsedVariable VName VValue

type VName = String

data VValue
  = Unquoted VContents
  | SingleQuoted String
  | DoubleQuoted VContents

type VContents = [VFragment]

data VFragment
  = VInterpolation String
  | VLiteral String

interpolateParsedVariables :: [ParsedVariable] -> IO [(String, String)]
interpolateParsedVariables = foldM addInterpolated []

addInterpolated :: [(String, String)] -> ParsedVariable -> IO [(String, String)]
addInterpolated previous (ParsedVariable name value) = (: previous) <$> ((,) name <$> interpolate previous value)

interpolate :: [(String, String)] -> VValue -> IO String
interpolate _        (SingleQuoted contents) = return contents
interpolate previous (DoubleQuoted contents) = interpolateContents previous contents
interpolate previous (Unquoted     contents) = interpolateContents previous contents

interpolateContents :: [(String, String)] -> VContents -> IO String
interpolateContents previous contents = concat <$> mapM (interpolateFragment previous) contents

interpolateFragment :: [(String, String)] -> VFragment -> IO String
interpolateFragment _        (VLiteral       value  ) = return value
interpolateFragment previous (VInterpolation varname) = fromPreviousOrEnv >>= maybe (return "") return
  where
    fromPreviousOrEnv = (lookup varname previous <|>) <$> lookupEnv varname
