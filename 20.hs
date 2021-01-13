module FoldableStudy where

import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- >>> sum [1,2,3]
-- 6

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- >>> product [1,2,8]
-- 16

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==x))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a = foldr go False
  where go x xs | x == a = True
                | otherwise = xs

-- >>> elem 1 [3,2,1]
-- True

-- >>> elem 4 [3,2,1]
-- False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where go x (Just y) = Just $ min x y
        go x Nothing  = Just x

-- >>> minimum [1,4,2,6,7,2]
-- Just 1
-- >>> minimum []
-- Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where go x (Just y) = Just $ max x y
        go x Nothing  = Just x

-- >>> maximum [1,4,2,6,7,2]
-- Just 7
-- >>> minimum []
-- Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- >>> null []
-- True

-- >>> null [1]
-- False

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ rest -> 1 + rest) 0

length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (const $ Sum 1)

-- >>> length [1,2,3]
-- 3

-- >>> length [1,2,3]
-- 3

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []


-- 9. Hint: use foldMap.
-- | Combine the elements
-- of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> f x <> y) mempty

------------------

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap _ _ = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four _ b b' b'') = f b <> f b' <> f b''

filterF :: ( Applicative f , Foldable t , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap go
  where go x | p x = pure x
             | otherwise = mempty

-- >>> filterF (>3) [1,9,2,8,3,7] :: [Int]
-- [9,8,7]

-- >>> filterF (>3) $ Just 5 :: [Int]
-- [5]

-- >>> filterF (>3) $ Just 1 :: [Int]
-- []
