{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist
import qualified Data.DList as DL
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- Six-sided die
data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use 'error'
  -- _extremely_ sparingly.
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
        (count + 1) nextGen

-- >>> rollsToGetTwenty (mkStdGen 17)
-- 4

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go (0, []) 0 g
  where
    go :: (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
    go (sum, hist) count gen
      | sum >= limit = (count, hist)
      | otherwise =
        let (die, nextGen) =
              randomR (1, 6) gen
        in go ((sum + die), intToDie die : hist) (count + 1) nextGen

--------------------------------------------------------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s
                               in (f a, s')

-- >>> runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
-- (1,0)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (f', s') = f s
                                        (a, s'') = g s'
                                    in (f' a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                  (Moi f') = g a
                              in f' s'

--------------------------------------------------------------------------------


fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [to, to-1..from]

-- >>> fizzbuzzFromTo 1 100
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz","Fizz","22","23","Fizz","Buzz","26","Fizz","28","29","FizzBuzz","31","32","Fizz","34","Buzz","Fizz","37","38","Fizz","Buzz","41","Fizz","43","44","FizzBuzz","46","47","Fizz","49","Buzz","Fizz","52","53","Fizz","Buzz","56","Fizz","58","59","FizzBuzz","61","62","Fizz","64","Buzz","Fizz","67","68","Fizz","Buzz","71","Fizz","73","74","FizzBuzz","76","77","Fizz","79","Buzz","Fizz","82","83","Fizz","Buzz","86","Fizz","88","89","FizzBuzz","91","92","Fizz","94","Buzz","Fizz","97","98","Fizz","Buzz"]

--------------------------------------------------------------------------------

get' :: Moi s s
get' = Moi $ \s -> (s,s)

-- >>> runMoi get' "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

put' :: s -> Moi s ()
put' s = Moi $ \_ -> ((), s)

-- >>> runMoi (put' "blah") "woot"
-- ((),"blah")

exec' :: Moi s a -> s -> s
exec' (Moi sa) = snd . sa

-- >>> exec' (put' "wilma") "daphne"
-- "wilma"

-- >>> exec' get' "scooby papu"
-- "scooby papu"

eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

-- >>> eval' get' "bunnicula"
-- "bunnicula"
-- >>> eval' get' "stake a bunny"
-- "stake a bunny"

modify'' :: (s -> s) -> Moi s ()
modify'' f = Moi $ \s -> ((), f s)

-- >>> runMoi (modify'' (+1)) 0
-- ((),1)
-- >>> runMoi (modify'' (+1) >> modify'' (+1)) 0
-- ((),2)
