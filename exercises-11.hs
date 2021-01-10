{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.Tuple
import Data.Maybe

-------------------------------------------------------------------------------
-- 9

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, n') = (n + n') > 42

-------------------------------------------------------------------------------
-- 17


data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- >>> mapOkay
-- "yup okay!"

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (inorder left) ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- >>> testPreorder
-- Preorder fine!
-- >>> testInorder
-- Inorder fine!
-- >>> testPostorder
-- Postorder fine!

foldTree :: (a -> b -> b)
  -> b
  -> BinaryTree a
  -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

listify = foldTree (\x acc -> acc++[x]) [] testTree

-- >>> listify
-- [2,1,3]

-------------------------------------------------------------------------------
-- 18

vig
  :: String -- ^ original
  -> String -- ^ key
  -> String -- ^ output
vig orig key = replace orig $ cycle key
  where
    replace [] (x:xs) = []
    replace (' ':rest) xs = ' ' : replace rest xs
    replace (c:rest) (x:xs) = adjust c x : replace rest xs
    adjust c offset = chr $ 97 + mod ((ord c - 97) + (ord offset - 97)) 26

-- >>> vig "meet at dawn" "ally"
-- "mppr ae oywy"
-- >>> vig "the quick brown fox jumped over the lazy dog" "ally"
-- "tsp outni bczun qzv jfxneo ztec efe wlxy oze"

-------------------------------------------------------------------------------
--- Phone one

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [((Digit, Presses), Char)]

daPhone :: DaPhone
daPhone = DaPhone
  [ (('2',1), 'a')
  , (('2',2), 'b')
  , (('2',3), 'c')
  , (('3',1), 'd')
  , (('3',2), 'e')
  , (('3',3), 'f')
  , (('4',1), 'g')
  , (('4',2), 'h')
  , (('4',3), 'i')
  , (('5',1), 'j')
  , (('5',2), 'k')
  , (('5',3), 'l')
  , (('6',1), 'm')
  , (('6',2), 'n')
  , (('6',3), 'o')
  , (('7',1), 'p')
  , (('7',2), 'q')
  , (('7',3), 'r')
  , (('7',4), 's')
  , (('8',1), 't')
  , (('8',2), 'u')
  , (('8',3), 'v')
  , (('9',1), 'w')
  , (('9',2), 'x')
  , (('9',3), 'y')
  , (('9',4), 'z')
  , (('0',1), '+')
  , (('0',2), '_')
  , (('#',1), '.')
  , (('#',2), ',')
  ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keypad) c = capMap $ fromJust $ lookup c $ map swap keypad
  where
    capMap :: (Digit, Presses) -> [(Digit, Presses)]
    capMap keypress
      | isUpper c = [('*', 1), keypress]
      | otherwise = [keypress]

-- >>> reverseTaps daPhone 'c'
-- [('2',3)]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-------------------------------------------------------------------------------
-- hutton's razor


data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)


-- >>> eval (Add (Lit 1) (Lit 9001))
-- 9002

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)

-- >>> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
