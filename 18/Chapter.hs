module MonadBoye where

import Prelude hiding (Right, Left)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Notes and exercises for chapter 18.

-------------------------------------------------------------------------------
-- List Monad

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

-- >>> twiceWhenEven [1,2,3,4]
-- [1,4,4,9,16,16]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []

-- >>> twiceWhenEven' [1,2,3,4]
-- [4,4,16,16]

-------------------------------------------------------------------------------
-- 18.7 Exercises

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  m >>= a = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope :: IO ()
testNope = do
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


----------------------------------------

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right b) = Right b
  fmap f (Left a) = Left $ f a

instance Applicative (PhhhbbtttEither b)where
  pure = Left
  Left f <*> Left x = Left $ f x
  Right x <*> _ = Right x
  _ <*> Right x = Right x

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left x >>= f = f x
  Right x >>= _ = Right x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [ (1, Left <$> arbitrary)
                        , (1, Right <$> arbitrary)
                        ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

testPe :: IO ()
testPe = do
  let trigger :: PhhhbbtttEither (Int, String, Int) (String, Int, String)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

----------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

testId :: IO ()
testId = do
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Semigroup (List a) where
  l <> l' =
    case l of
      Nil -> l'
      Cons x xs -> Cons x (xs <> l')

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil       <*> _   = Nil
  _         <*> Nil = Nil
  Cons f fs <*> xs  = fmap f xs <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil      >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [ (1, Cons <$> arbitrary <*> arbitrary)
                        , (1, pure Nil)
                        ]

testList :: IO ()
testList = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--------------------------------------------------------------------------------

j :: Monad m => m (m a) -> m a
j x = x >>= id -- == join

-- >>> j [[1, 2], [], [3]]
-- [1,2,3]

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5. You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

nullHater :: Int -> Maybe Int
nullHater x | x > 0 = Just 1
            | x <= 0 = Nothing

-- >>> meh [1,2,3] nullHater
-- Just [1,1,1]

-- >>> meh [1,0,3] nullHater
-- Nothing

-- 6. Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

-- >>> flipType [Just 1, Just 2]
-- Just [1,2]

-- >>> flipType [Just 1, Nothing]
-- Nothing

-------------------------------------------------------------------------------

main = testList
