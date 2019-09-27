-------------------------------------------------------------------------------
-- 6

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123)
           )
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123)
           )
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldl f []
  where
    f acc x
      | DbDate v <- x = acc ++ [v]
      | otherwise = acc

-- >>> let l = [DbString "one", DbNumber 2, DbDate (UTCTime (toEnum 1) (toEnum 1))]
-- >>> filterDbDate l
-- [1858-11-18 00:00:00.000000000001 UTC]

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldl f []
  where
    f acc x
      | DbNumber v <- x = acc ++ [v]
      | otherwise = acc

-- >>> let l = [DbString "one", DbNumber 2, DbDate (UTCTime (toEnum 1) (toEnum 1))]
-- >>> filterDbNumber l
-- [2]

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = maximum . filterDbDate --partial but the fuckers asked for it

-- >>> let l = [DbDate (UTCTime (toEnum 1) (toEnum 1)), DbDate (UTCTime (toEnum 2) (toEnum 2))]
-- >>> mostRecent l
-- 1858-11-19 00:00:00.000000000002 UTC

sumDb :: [DatabaseItem]
      -> Integer
sumDb = sum . filterDbNumber

-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem]
      -> Double
avgDb xs = fromIntegral (sum $filterDbNumber xs) / (fromIntegral $ length xs)

-------------------------------------------------------------------------------
-- 9

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- >>> fibsN 6
-- 13

fibs' = take 20 fibs

-- >>> fibs'
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

fibs'' = takeWhile (<100) fibs

-- >>> fibs''
-- [1,1,2,3,5,8,13,21,34,55,89]

-------------------------------------------------------------------------------
-- 10

stops = "pbtdkg"
vowels = "aeiou"
allWords = [[c, v, c'] | c <- stops, v <- vowels, c' <- stops]

-- >>> take 5 allWords
-- ["pap","pab","pat","pad","pak"]

myAnd :: [Bool] -> Bool
myAnd = foldr (\x y -> if x == False then False else y) True

-- >>> myAnd [True, True, True, False, True]
-- False

myOr :: [Bool] -> Bool
myOr = foldr (\x y -> if x == True then True else y) False

-- >>> myOr [False, False, True, False]
-- True

-- 2. myAny returns True if a -> Bool applied to any of the values in the
-- list returns True.

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> if f x then True else y) False

-- >>> myAny even [1, 2, 3]
-- True

-- 3. Write two versions of myElem. One version should use folding
-- and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x y -> if e == x then True else y) False
-- >>> myElem 4 [1, 2, 3]
-- False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = myAny . (==)
-- >>> myElem' 2 [1, 2, 3]
-- True

-- 4. Implement myReverse, don’t worry about trying to make it
-- lazy.
myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []
-- >>> myReverse [1,2,3]
-- [3,2,1]

-- 5. Write myMap in terms of foldr. It should have the same behavior
-- as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []
-- >>> myMap even [1,2,3,4,5]
-- [False,True,False,True,False]

-- 6. Write myFilter in terms of foldr. It should have the same behav-
-- ior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []
-- >>> myFilter even [1,2,3,4,5]
-- [2,4]

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []
-- >>> squish [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]

-- 8. squishMap maps a function over a list and concatenates the re-
-- sults.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (myMap f)
-- Prelude> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- Prelude> let f x = "WO " ++ [x] ++ " OT "
-- Prelude> squishMap f "blah"
-- "WO b OT WO l OT WO a OT WO h OT "

-- 9. squishAgain flattens a list of lists into a list. This time re-use the
-- squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = undefined

-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
-- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- 384
-- 10
-- Prelude> myMaximumBy compare [1..10]
-- 10

-- 11. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the
-- comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
-- Prelude> myMinimumBy (\_ _ -> GT) [1..10]
-- 10
-- Prelude> myMinimumBy (\_ _ -> LT) [1..10]
-- 1
-- Prelude> myMinimumBy compare [1..10]
-- 1
