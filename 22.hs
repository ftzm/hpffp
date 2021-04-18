-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Data.Char
--
import Control.Applicative
import Data.Maybe
import Prelude hiding (lookup)

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person
  (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- >>> getDogRM pers
-- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

-- >>> runReader getDogRM' pers
-- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- >>> composed "julie"
-- "EILUJ"

-- >>> composed "chris"
-- "SIRHC"

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

-- >>> tupled "julie"
-- ("JULIE","eiluj")

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return (c,r)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = runReader ((Reader cap) >>= (\c -> (Reader rev) >>= (\r -> Reader $ const (c, r))))

-- >>> tupledM' "julie"
-- ("JULIE","eiluj")

--------------------------------------------------------------------------------
-- Exercises

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup target [] = Nothing
lookup target ((k,v):rest)
  | target == k = Just v
  | otherwise = lookup target rest

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- >>> xs
-- Just 6

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- >>> ys
-- Just 9

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- >>> zs
-- Nothing

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- >>> x1
-- Just (6,9)

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- >>> x2
-- Nothing

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

-- >>> x3 3
-- (Just 9,Just 9)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d (Just v) = v
fromMaybe' d Nothing = d

main1 :: IO ()
main1 = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldl (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys

-- >>> main1
-- Just [3,2,1]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- Just [6,9]
-- Just 15
-- Nothing
-- True
-- [True,False,False]
-- [True,True,False]
-- False
-- [True,False,False]
-- False

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)
