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
import           Control.Monad                    (foldM)
import           Control.Applicative              ((<|>))
import           System.Process                   (readCreateProcess, shell)

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
  | VarLiteral String
  | CommandInterpolation String deriving (Show, Eq)

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
interpolateFragment _ (CommandInterpolation commandName) = init <$> readCreateProcess (shell commandName) ""

joinContents :: VarContents -> String
joinContents = concatMap fragmentToString
  where
    fragmentToString (CommandInterpolation value) = value
    fragmentToString (VarInterpolation value)     = value
    fragmentToString (VarLiteral value)           = value
