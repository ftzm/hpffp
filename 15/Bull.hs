module Bull where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  r@(First' (Only a)) <> _ = r
  _ <> r@(First' (Only a)) = r
  _ <> _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend
  :: First' a
  -> First' a
  -> First' a
firstMappend = mappend

type FirstMappend
  =  First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Only <$> arbitrary]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main' :: IO ()
main' = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-------------------------------------------------------------------------------
-- Exercises
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-------------------------------------------------------------------------------
-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Semigroup a) => Semigroup (Identity a) where
  Identity x <> Identity x' = Identity $ x <> x'

type IdentAssoc a = Identity a -> Identity a -> Identity a -> Bool

-------------------------------------------------------------------------------
-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-------------------------------------------------------------------------------
-- 4 + 5
-- Basically the same as 3, fuck that shit, I get it.

-------------------------------------------------------------------------------
-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _             <> _             = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-------------------------------------------------------------------------------
-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance Semigroup BoolDisj where
  BoolDisj True  <> _             = BoolDisj True
  _              <> BoolDisj True = BoolDisj True
  _              <> _             = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-------------------------------------------------------------------------------
-- 8

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)=> Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

instance Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-------------------------------------------------------------------------------
-- 9

-- This one is actually a bit bullshit; readers of the book will likely not
-- have the tools to complete this successfully, and will be confused at the
-- suggestion that this is possible for them.

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine f' = Combine $ \x -> f x <> f' x

-- Help for the below was found here:
-- https://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type

funEquality :: (Arbitrary a, Show a, Eq b, Show b)
            => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) = property $ \a -> f a === g a

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

combineAssoc :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))

-------------------------------------------------------------------------------
-- 10

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp f' = Comp $ f . f'

---

funEquality' :: (Arbitrary a, Show a, Eq a, Show a)
            => Comp a -> Comp a -> Property
funEquality' (Comp f) (Comp g) = property $ \a -> f a === g a

type CompAssoc a = Comp a -> Comp a -> Comp a -> Property

compAssoc :: (Arbitrary a, Show a, Eq a, Semigroup a) => CompAssoc a
compAssoc f g h = ((f <> g) <> h) `funEquality'` (f <> (g <> h))

-------------------------------------------------------------------------------
-- 11

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Failure' <$> arbitrary, Success' <$> arbitrary]

instance Semigroup a => Semigroup (Validation a b) where
  Failure' a <> Failure' a' = Failure' $ a <> a'
  Success' b <> _           = Success' b
  _          <> Success' b' = Success' b'

type ValAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-------------------------------------------------------------------------------
-- Run all Semigroup

main2 :: IO ()
main2 = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String [Int])
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String [Int])
  quickCheck $ \(Fn f) (Fn g) (Fn h) -> (combineAssoc :: CombineAssoc Int All) (Combine f) (Combine g) (Combine h)
  quickCheck $ \(Fn f) (Fn g) (Fn h) -> (compAssoc :: CompAssoc All) (Comp f) (Comp g) (Comp h)
  quickCheck (semigroupAssoc :: ValAssoc String Int)

-------------------------------------------------------------------------------
-- Monoid Exercises

-------------------------------------------------------------------------------
-- 1

instance Monoid Trivial where
  mempty = Trivial

-------------------------------------------------------------------------------
-- 2

instance Monoid a => Monoid (Identity a) where
  mempty = Identity $ mempty

-------------------------------------------------------------------------------
-- 3

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-------------------------------------------------------------------------------
-- 4

instance Monoid (BoolConj) where
  mempty = BoolConj True

-------------------------------------------------------------------------------
-- 5

instance Monoid (BoolDisj) where
  mempty = BoolDisj False

-------------------------------------------------------------------------------
-- 6

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

-------------------------------------------------------------------------------
-- 7

instance Monoid (Comp a) where
  mempty = Comp id

-------------------------------------------------------------------------------
-- 8

newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' = Mem $ \s ->
    let
      (a, s') = f s
      (a', s'') = f' s'
    in (a <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

memTest = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-------------------------------------------------------------------------------
-- Run all Monoid

main :: IO ()
main = do
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (monoidLeftIdentity :: Identity All -> Bool)
  quickCheck (monoidRightIdentity :: Identity All -> Bool)

  quickCheck (monoidLeftIdentity :: Two All String -> Bool)
  quickCheck (monoidRightIdentity :: Two All String -> Bool)

  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  -- 6 can't be arsed
  -- 7 can't be arsed
