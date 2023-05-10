-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers to interpolate environment variables

module Configuration.Dotenv.ParsedVariable (ParsedVariable(..),
                                            VarName,
                                            VarValue(..),
                                            VarContents,
                                            VarFragment(..),
                                            interpolateParsedVariables) where

import           Configuration.Dotenv.Environment (lookupEnv)
import           Control.Applicative              ((<|>))
import           Control.Monad                    (foldM)
import           System.Process                   (proc, readCreateProcess)

-- | Name and value pair
data ParsedVariable
  = ParsedVariable VarName VarValue deriving (Show, Eq)

-- | Variable name
type VarName = String

-- | Possible state of values
data VarValue
  = Unquoted VarContents
  | SingleQuoted VarContents
  | DoubleQuoted VarContents deriving (Show, Eq)

-- | List of VarFragment
type VarContents = [VarFragment]

-- | Placeholder for possible values
data VarFragment
  = VarInterpolation String
  | VarLiteral String
  | CommandInterpolation String [String] deriving (Show, Eq)

-- | Interpotales parsed variables
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
interpolateFragment _ (CommandInterpolation commandName args) = init <$> readCreateProcess (proc commandName args) ""

joinContents :: VarContents -> String
joinContents = concatMap fragmentToString
  where
    fragmentToString (CommandInterpolation commandName args) = unwords $ commandName : args
    fragmentToString (VarInterpolation value)                = value
    fragmentToString (VarLiteral value)                      = value
