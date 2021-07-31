{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter26 where

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
