{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Chapter26 where

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Functor.Identity

--------------------------------------------------------------------------------
-- EitherT

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- Just for sanity checking
instance Show (m (Either e a)) => Show (EitherT e m a) where
  showsPrec d (EitherT m) = showParen (d > 10) $
    showString "EitherT " . showsPrec 11 m

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT fga) = EitherT $ (fmap . fmap) f fga

e = EitherT $ Just $ Right 1
-- >>> fmap (+1) e
-- EitherT (Just (Right 2))

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

-- >>> pure 1 :: EitherT String Maybe Int
-- EitherT (Just (Right 1))

-- >>> (pure (+1)) <*> (pure 1) :: EitherT String Maybe Int
-- EitherT (Just (Right 2))

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left e -> return $ Left e
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither ma

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT  f g (EitherT ma) = do
  v <- ma
  case v of
    Left a -> f a
    Right b -> g b

--------------------------------------------------------------------------------
-- StateT

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> fmap (\(a, s') -> (f a, s')) (g s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT a) <*> (StateT b) = StateT $ \s -> do
    (f, s') <- a s
    (x, s'') <- b s'
    return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT (sma) >>= f = StateT $ \s -> do
    (x, s') <- sma s
    runStateT (f x) s'

--------------------------------------------------------------------------------
-- Wrap it up

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

-- exercise originally broken for lack of `return` in original
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT (const (return (Right (Just 1))))


--------------------------------------------------------------------------------
-- Lift More

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> fmap (,s) m

--------------------------------------------------------------------------------
-- LiftIO

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Functor, Applicative, Monad)


class (Monad m) => MonadIO m where
-- | Lift a computation
-- from the 'IO' monad.
  liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
