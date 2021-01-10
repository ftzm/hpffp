import Data.List (partition)

--------------------------------------------------------------------------------
-- 5

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . flip replace [] . words
  where
    replace :: [String] -> [String] -> [String]
    replace acc [] = acc
    replace acc (x:xs) = replace xs $ acc ++ case notThe x of
      Just s -> [s]
      Nothing -> ["a"]

-- >>> replaceThe "I'm the man"
-- "I'm a man"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count 0 . words
  where
    count :: Integer -> [String] -> Integer
    count acc (w1:w2:xs)
      | (w1 == "the") && (head w2 `elem` "aeiouy") = count (acc + 1) xs
      | otherwise = count acc xs
    count acc _ = acc

-- >>> countTheBeforeVowel "the cow"
-- 0

-- >>> countTheBeforeVowel "the evil cow"
-- 1

countVowels :: String -> Integer
countVowels = toInteger . length . filter (flip elem "aieou")

-- >>> countVowels "the cow"
-- 2

-- >>> countVowels "Mikolajczaki"
-- 5

--------------------------------------------------------------------------------

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if uncurry (>) $ partition (`elem` vowels) s
  then Nothing
  else Just $ Word' s

-- >>> mkWord "test"
-- Just (Word' "test")

-- >>> mkWord "ono"
-- Nothing

--------------------------------------------------------------------------------

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ rest) = 1 + natToInteger rest

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ convert i
  where
    convert 0 = Zero
    convert i = (Succ (convert (i - 1)))

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing

--------------------------------------------------------------------------------

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f m = case m of
  Nothing -> b
  Just a -> f a

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2

fromMaybe :: a -> Maybe a -> a
fromMaybe x m = mayybee x id m

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1

--------------------------------------------------------------------------------

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) acc = acc ++ [x]
    f (Right _) acc = acc

-- >>> lefts' [Left 1, Right 'c']
-- [1]

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) acc = acc ++ [x]
    f (Left _) acc = acc

-- >>> rights' [Left 1, Right 'c']
-- "c"

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr f ([], [])
  where
    f (Left a) (as, bs) = (as ++ [a], bs)
    f (Right b) (as, bs) = (as, bs ++ [b])

-- >>> partitionEithers [Left 1, Right 'c']
-- ([1],"c")

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf rf e = case e of
  Left x -> lf x
  Right x -> rf x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (const Nothing) (Just . f) e

--------------------------------------------------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- >>> take 10 $ myIterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (y, f y)) x

-- >>> take 10 $ betterIterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]

--------------------------------------------------------------------------------

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (s1, v, s2) -> Node (unfold f s1) v (unfold f s2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild i = unfold f 0
  where
    f x | x < 0 = Nothing
        | x == i = Nothing
        | otherwise = Just (x+1, x, x+1)

-- >>> treeBuild 0
-- Leaf

-- >>> treeBuild 1
-- Node Leaf 0 Leaf

-- >>> treeBuild 2
-- Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)

-- >>> treeBuild 3
