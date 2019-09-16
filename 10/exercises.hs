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
