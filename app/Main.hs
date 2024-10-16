{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Version         (showVersion)
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid          ((<>))
#endif

import           Options.Applicative
import           Paths_dotenv         (version)

import           Configuration.Dotenv (Config (..), defaultConfig, loadFile)
import           System.Directory     (doesFileExist)
import           System.Exit          (exitWith)
import           System.Process       (system)

data Options = Options
  { dotenvFiles        :: [String]
  , dotenvExampleFiles :: [String]
  , override           :: Bool
  , verbose            :: Bool
  , dryRun             :: Bool
  , duplicates         :: Bool
  , program            :: String
  , args               :: [String]
  } deriving (Show)

data Flags = Flags
  { flagDotenvFiles        :: [String]
  , flagDotenvExampleFiles :: [String]
  , flagOverride           :: Bool
  , flagVerbose            :: Bool
  , flagDryRun             :: Bool
  , flagDuplicates         :: Bool
  }

defaultFlags :: Flags
defaultFlags = Flags
  { flagDotenvFiles = []
  , flagDotenvExampleFiles = []
  , flagOverride = False
  , flagVerbose = False
  , flagDryRun = False
  , flagDuplicates = False
  }

applyFlags :: Options -> Flags -> Options
applyFlags options flags = Options
  { dotenvFiles = dotenvFiles options <> flagDotenvFiles flags
  , dotenvExampleFiles = dotenvExampleFiles options <> flagDotenvExampleFiles flags
  , override = override options || flagOverride flags
  , verbose = verbose options || flagVerbose flags
  , dryRun = dryRun options || flagDryRun flags
  , duplicates = duplicates options || flagDuplicates flags
  , program = program options
  , args = args options
  }

main :: IO ()
main = do
  options <- execParser $ mkOpts config
  flags <- loadFlagsFromConfig
  let Options{..} = applyFlags options flags
  let configDotenv =
        Config
          { configExamplePath = dotenvExampleFiles
          , configOverride = override
          , configVerbose = verbose
          , configDryRun = dryRun
          , allowDuplicates = duplicates
          , configPath =
              if null dotenvFiles
                then configPath defaultConfig
                else dotenvFiles
          }
   in do
    loadFile configDotenv
    if configDryRun configDotenv
      then putStrLn "[INFO]: Dry run mode enabled. Not executing the program."
      else system (program ++ concatMap (" " ++) args) >>= exitWith

mkOpts :: Parser a -> ParserInfo a
mkOpts p = info (helper <*> versionOption <*> p)
    ( fullDesc
    <> progDesc "Runs PROGRAM after loading options from FILE"
    <> header "dotenv - loads options from dotenv files" )
    where
        versionOption = infoOption
             (showVersion version)
             (long "version" <> short 'v' <> help "Show version of the program")

config :: Parser Options
config = Options
     <$> dotenvFileOption
     <*> exampleFileOption
     <*> overloadOption
     <*> verboseOption
     <*> dryRunOption
     <*> noDuplicatesOption
     <*> argument str (metavar "PROGRAM")
     <*> many (argument str (metavar "ARG"))

dotenvFileOption :: Parser [String]
dotenvFileOption = many
    ( strOption (
        long "dotenv"
        <> short 'f'
        <> metavar "DOTENV"
        <> help "File to read for environmental variables" ))

exampleFileOption :: Parser [FilePath]
exampleFileOption =  many
    ( strOption (
        long "example"
        <> short 'x'
        <> metavar "DOTENV_EXAMPLE"
        <> help "File to read for needed environmental variables" ))

overloadOption :: Parser Bool
overloadOption = switch
    ( long "overload"
    <> short 'o'
    <> help "Specify this flag to override existing variables" )

verboseOption :: Parser Bool
verboseOption = switch
    ( long "verbose"
    <> help "Specify this flag to print out the variables loaded and other useful insights" )

dryRunOption :: Parser Bool
dryRunOption = switch
    (  long "dry-run"
    <> help "Specify this flag to print out the variables loaded without executing the program" )

noDuplicatesOption :: Parser Bool
noDuplicatesOption = flag True False
    ( long "no-dups"
    <> short 'D'
    <> help "Specify this flag to forbid duplicate variables" )

flagsP :: Parser Flags
flagsP = Flags
     <$> dotenvFileOption
     <*> exampleFileOption
     <*> overloadOption
     <*> verboseOption
     <*> dryRunOption
     <*> noDuplicatesOption

configFile :: FilePath
configFile = "dotenv.config"

loadFlagsFromConfig :: IO Flags
loadFlagsFromConfig = do
    configExists <- doesFileExist configFile
    if not configExists then return defaultFlags
    else do
        arguments <- words <$> readFile configFile
        case execParserPure defaultPrefs (mkOpts flagsP) arguments of
            result@(Failure _) -> do
                putStrLn "There were errors while parsing the configuration file"
                handleParseResult result
            result -> handleParseResult result
