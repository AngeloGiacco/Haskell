import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- Problem 55
-- Completely Balanced Trees
balTree :: Int -> [Tree Char]
balTree 0 = [Empty] 
balTree n = [Branch 'x' left right | i     <- [q .. q + r],
                                     left  <- balTree i,
                                     right <- balTree (n - 1 - i)]
  where 
      (q, r) = (n - 1) `quotRem` 2

-- Problem 56
-- Is symmetric tree?

symTree :: Tree a -> Bool 
symTree Empty = True
symTree (Branch _ l r) = symmetric l r 

symmetric :: Tree a -> Tree a -> Bool 
symmetric Empty Empty                   = True 
symmetric (Branch _ a b) (Branch _ x y) = symmetric a y && symmetric b x 
symmetric _ _                           = False

-- Problem 57
-- build binary tree

construct :: Ord a => [a] -> Tree a
construct xs = construct' xs Empty
  where 
    construct' :: Ord a => [a] -> Tree a -> Tree a 
    construct' [] t     = t 
    construct' (x:xs) t = construct' xs (insert x t)

    insert :: Ord a => a -> Tree a -> Tree a 
    insert x Empty = Branch x Empty Empty 
    insert x (Branch cur l r)
      | x < cur   = Branch cur (insert x l) r 
      | otherwise = Branch cur l (insert x r)

-- Problem 58

isBalTree :: Tree a -> Bool 
isBalTree (Branch _ l r) = almostEqual (numNodes l) (numNodes r)
  where 
      numNodes :: Tree a -> Int 
      numNodes (Branch _ l r) = 1 + numNodes l + numNodes r 
      numNodes Empty          = 1

      almostEqual :: Int -> Int -> Bool 
      almostEqual x y = x <= y + 1 && y <= x + 1

genAllSymBalTrees :: Int -> [Tree Int] 
genAllSymBalTrees n = filter (\x -> symTree x && isBalTree x) $ nub $ map construct $ permutations [1..n] 

height :: Tree a -> Int 
height Empty = 0 
height (Branch _ l r) = 1 + max (height l) (height r)

-- Problem 61 
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1 
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a] 
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r 

-- Problem 62
internalNodes :: Tree a -> [a] 
internalNodes Empty = []
internalNodes (Branch _ Empty Empty) = []
internalNodes (Branch x l r) = (x : internalNodes l) ++ internalNodes r

atLevel :: Int -> Tree a -> [a] 
atLevel _ Empty = []
atLevel 1 (Branch x _ _) = [x]
atLevel n (Branch _ l r) = atLevel (n-1) l ++ atLevel (n-1) r

ipl :: Tree a -> Int 
ipl = ipl' 0 
  where 
    ipl' :: Int -> Tree a ->  Int 
    ipl' _ Empty                  = 0
    ipl' n (Branch _ Empty Empty) = n 
    ipl' n (Branch _ l r)         = n + ipl' (n + 1) l  + ipl' (n + 1) r 

bottomUp :: Tree a -> [a]
bottomUp (Branch x Empty Empty) = [x]
bottomUp (Branch x l r) = bottomUp l ++ bottomUp r ++ [x]