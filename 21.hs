{-# LANGUAGE FlexibleContexts #-}

module Chapter21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--------------------------------------------------------------------------------
-- Identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


--------------------------------------------------------------------------------
-- Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

--------------------------------------------------------------------------------
-- Optional

data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

--------------------------------------------------------------------------------
-- List

data List a = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f list = case list of
    Nil -> Nil
    Cons x xs -> Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x rest) = f x <> foldMap f rest

-- >>> foldMap Sum (Cons 1 (Cons 1 Nil))
-- Sum {getSum = 2}

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x rest) = Cons <$> f x <*> traverse f rest

-- >>> traverse Identity (Cons 1 (Cons 1 Nil))
-- Identity (Cons 1 (Cons 1 Nil))

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [ (1, Cons <$> arbitrary <*> arbitrary)
                        , (1, pure Nil)
                        ]

--------------------------------------------------------------------------------
-- Three

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- Big

data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance  Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- S

data S n a = S (n a) a
  deriving (Eq, Show)

instance ( Functor n , Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n , Testable (n Property) , EqProp a ) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance ( Functor n) => Functor (S n) where
  fmap f (S x y) = S (f <$> x) $ f y

instance ( Foldable n ) => Foldable (S n) where
  foldMap f (S x y) = foldMap f x <> f y

instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

--main = sample' (arbitrary :: Gen (S [] Int))

--------------------------------------------------------------------------------
-- Tree

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r ) = Node <$> traverse f l <*> f x <*> traverse f r

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    elements [Empty, Leaf a, Node l a r]

--------------------------------------------------------------------------------
-- Main the First

type TI = Tree

main = do
  let trigger :: TI  (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
