module Configuration.Dotenv.ParsedVariable (ParsedVariable(..),
                                            VName,
                                            VValue(..),
                                            VContents,
                                            VFragment(..)) where

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
