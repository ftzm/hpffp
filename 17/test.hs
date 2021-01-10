{-# TupleSections #-}

import Data.Monoid
import Data.List (elemIndex)
import Control.Applicative

app = (\x y z -> x + y + z) <$> Just 1 <*> Just 1 <*> Just 1

l = liftA3 (\x y z -> x + y + z) (Just 1) (Just 1) (Just 1)

-- >>> app
-- Just 3

-- >>> l
-- Just 3

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y' :: Maybe Integer
y' = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed =  sum <$> ((,) <$> x'' <*> y'')

-- >>> summed
-- Just 5

-------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity $ f x

xxs = [1, 2, 3]
xxs' = [9, 9, 9]
mkId = Identity
testeroni = const <$> mkId xxs <*> mkId xxs'
-- Identity [1,2,3]

-- >>> testeroni
-- Identity [1,2,3]

-------------------------------------------------------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x <> y

f = Constant (Sum 1)
g = Constant (Sum 2)

-- >>> f <*> g
-- Constant {getConstant = Sum {getSum = 3}}

-------------------------------------------------------------------------------
-- List Applicative

data List a = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  l <> l' =
    case l of
      Nil -> l'
      Cons x xs -> Cons x (xs <> l')


foldList :: (a -> b -> b) -> b -> List a -> b
foldList f acc Nil = acc
foldList f acc (Cons x xs) = f x (foldList f acc xs)

instance Functor List where
  fmap f list = case list of
    Nil -> Nil
    Cons x xs -> Cons (f x) (fmap f xs)

-- >>> fmap (+1) $ Cons 1 (Cons 2 Nil)
-- Cons 2 (Cons 3 Nil)

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = foldList (<>) Nil $ fmap (\x -> fmap (\f -> f x) fs) xs

-- >>> let f = Cons (+1) (Cons (*2) Nil)
-- >>> let v = Cons 1 (Cons 2 Nil)
-- >>> f <*> v
-- Cons 2 (Cons 2 (Cons 3 (Cons 4 Nil)))

-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))) <-- Correct

-------------------------------------------------------------------------------
-- ZipList Applicative


take' :: Int -> List a -> List a
take' = undefined

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a Nil
  ZipList' fs' <*> ZipList' xs' = case fs' of
    Nil -> ZipList' Nil
    Cons f fs -> case xs' of
      Nil -> ZipList' Nil
      Cons x xs -> ZipList' $ Cons (f x) (fs <*> xs)

-- >>> ZipList' (Cons (+1) (Cons (*2) Nil)) <*> ZipList' (Cons 1 (Cons 2 Nil))
-- ZipList' (Cons 2 (Cons 4 Nil))

-------------------------------------------------------------------------------
-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

other :: String
other = "!$#"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- >>> combos stops vowels other
-- [('p','a','!'),('p','a','$'),('p','a','#'),('p','e','!'),('p','e','$'),('p','e','#'),('p','i','!'),('p','i','$'),('p','i','#'),('p','o','!'),('p','o','$'),('p','o','#'),('p','u','!'),('p','u','$'),('p','u','#'),('b','a','!'),('b','a','$'),('b','a','#'),('b','e','!'),('b','e','$'),('b','e','#'),('b','i','!'),('b','i','$'),('b','i','#'),('b','o','!'),('b','o','$'),('b','o','#'),('b','u','!'),('b','u','$'),('b','u','#'),('t','a','!'),('t','a','$'),('t','a','#'),('t','e','!'),('t','e','$'),('t','e','#'),('t','i','!'),('t','i','$'),('t','i','#'),('t','o','!'),('t','o','$'),('t','o','#'),('t','u','!'),('t','u','$'),('t','u','#'),('d','a','!'),('d','a','$'),('d','a','#'),('d','e','!'),('d','e','$'),('d','e','#'),('d','i','!'),('d','i','$'),('d','i','#'),('d','o','!'),('d','o','$'),('d','o','#'),('d','u','!'),('d','u','$'),('d','u','#'),('k','a','!'),('k','a','$'),('k','a','#'),('k','e','!'),('k','e','$'),('k','e','#'),('k','i','!'),('k','i','$'),('k','i','#'),('k','o','!'),('k','o','$'),('k','o','#'),('k','u','!'),('k','u','$'),('k','u','#'),('g','a','!'),('g','a','$'),('g','a','#'),('g','e','!'),('g','e','$'),('g','e','#'),('g','i','!'),('g','i','$'),('g','i','#'),('g','o','!'),('g','o','$'),('g','o','#'),('g','u','!'),('g','u','$'),('g','u','#')]
