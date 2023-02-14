-- |
-- Module      :  Configuration.Dotenv.Internal
-- Copyright   :  © 2015–2020 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides some helpers used internally

module Configuration.Dotenv.Internal
  ( ask
  , runReaderT
  , liftReaderT
  , ReaderT
  )
  where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    m >>= k  = ReaderT $ \ r -> do
      a <- runReaderT m r
      runReaderT (k a) r

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
