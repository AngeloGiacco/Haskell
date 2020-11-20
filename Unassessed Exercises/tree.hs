data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show,Eq)

size :: Tree a -> Int
size Empty= 0
size (Node l _ r)= 1 + size l + size r

flatten :: Tree a -> [a]
flatten Empty= []
flatten (Node t1 x t2) = flatten t1  ++  (x : flatten t2)

insert :: Int -> Tree Int -> Tree Int
-- Pre: The tree is ordered
insert n Empty= Node Empty n Empty
insert n (Node t1 x t2)
  | n <= x    = Node (insert n t1) x t2
  | otherwise = Node t1 x (insert n t2)

build :: [Int] -> Tree Int
build= foldr insert Empty

bfWalk :: Tree a -> [a]
bfWalk t
  = bfWalk' [t]
    where
      bfWalk' :: [Tree a] -> [a]
      bfWalk' []
        = []
      bfWalk' (Empty : ts)
        = bfWalk' ts
      bfWalk' (Node l x r : ts)
        = x : bfWalk' (ts ++ [l, r])

bflabel :: [Tree a] -> Int -> [Tree Int]
bflabel [] _ = []
bflabel (Empty:ts) n = Empty : bflabel ts n 
bflabel (Node l _ r : ts) n 
  = Node l' n r' : reverse ts'
    where (r':l':ts') = reverse (bflabel (ts ++ [l,r]) (n+1))


children :: Tree a -> [Tree a]
children Empty = []
children (Node l _ r) = [l,r]

childrenOf :: [Tree a] -> [Tree a]
childrenOf = concatMap children

invertTree :: Tree a -> Tree a 
invertTree Empty = Empty 
invertTree (Node left val right) = Node (invertTree right) val (invertTree left)

rangeSumBST :: Tree Int -> Int -> Int -> Int
rangeSumBST Empty _ _ = 0
rangeSumBST (Node left val right) l u
  | val < l    = rangeSumBST right l u
  | val > u    = rangeSumBST left l u
  | otherwise  = rangeSumBST left l u + rangeSumBST right l u + val 

labelBFLayer :: [Tree a] -> Int -> [Tree Int]
labelBFLayer [] _ = []
labelBFLayer ts n = labelRow ts (labelBFLayer (childrenOf ts) (n+k)) n 
  where k = sum [1| (Node _ _ _ ) <- ts]
        labelRow [] _ _ = []
        labelRow (Empty : ts) ts' n = Empty : labelRow ts ts' n
        labelRow (Node _ _ _ : ts) ~(cL : cR : cs) n
          = Node cL n cR : labelRow ts cs (n+1)

