-- |
-- Module      :  Configuration.Dotenv.Types
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides the types with extra options for loading a dotenv file.

module Configuration.Dotenv.Types
  ( Config(..)
  , defaultConfig
  , ask
  , runReaderT
  , liftReaderT
  , ReaderT
  )
  where

-- | Configuration Data Types with extra options for executing dotenv.
data Config = Config
  { configPath        :: [FilePath] -- ^ The paths for the .env files
  , configExamplePath :: [FilePath] -- ^ The paths for the .env.example files
  , configOverride    :: Bool     -- ^ Flag to allow override env variables
  , configVerbose     :: Bool     -- ^ Flag to log the loaded variables and other useful information
  , disallowDuplicates   :: Bool     -- ^ Flag to allow duplicate variables
  } deriving (Eq, Show)

-- | Default configuration. Use .env file without .env.example strict envs and
-- without overriding.
defaultConfig :: Config
defaultConfig =
  Config
    { configExamplePath = []
    , configOverride = False
    , configPath = [ ".env" ]
    , configVerbose = False
    , disallowDuplicates = True
    }


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m
instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)
instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    return   = liftReaderT . return
    m >>= k  = ReaderT $ \ r -> do
      a <- runReaderT m r
      runReaderT (k a) r
    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
