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
