import Data.List ( (\\) )

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)


--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node x _ _) = x

rank :: BinTree a -> Int
rank (Node _ x _) = x

children :: BinTree a -> [BinTree a]
children (Node _ _ x) = x

-- Pre trees have the same rank
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees a b
 | value a < value b = Node (value a) (rank a + 1) (b : children a)
 | otherwise         = Node (value b) (rank b + 1) (a : children b)     

  
--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a  
extractMin xs = value $ foldl1 min' xs
   
min' :: Ord a => BinTree a -> BinTree a -> BinTree a
min' a b = if value a < value b then a else b


mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h [] = h
mergeHeaps [] h = h
mergeHeaps (t:ts) (t':ts')
  | rank t < rank t' = t  : mergeHeaps ts  (t':ts')
  | rank t' < rank t = t' : mergeHeaps ts' (t:ts)
  | otherwise        = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts') 
  
insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val = mergeHeaps [Node val 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h = mergeHeaps minTreeHeap notMinHeap 
  where
    minTreeHeap           = reverse $ children minTree
    (minTree,notMinHeap)  = removeMin h    


remove :: Eq a => a -> BinHeap a -> BinHeap a
remove x (t:ts)
  | x == value t = ts 
  | otherwise    = t: remove x ts   

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h = (t,h')
    where 
        m  = extractMin h 
        h' = remove m h
        t  = head $ h \\ h'

binSort :: Ord a => [a] -> [a]
binSort = reverse . decompose [] . build []
 
build :: Ord a => BinHeap a -> [a] -> BinHeap a 
build = foldl (flip insert)

decompose :: Ord a => [a] -> BinHeap a -> [a]
decompose acc [] = acc 
decompose acc h  = decompose (extractMin h : acc) (deleteMin h)

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary xs = toBinary' 0 [] vals
  where 
      vals = map rank xs 
      toBinary' :: Int -> [Int] -> [Int] -> [Int]
      toBinary' _ acc []   = acc
      toBinary' n acc ones
        | n `elem` ones      = toBinary' (n+1) (1:acc) (ones \\ [n])
        | otherwise        = toBinary' (n+1) (0:acc) ones

binarySum :: [Int] -> [Int] -> [Int]
binarySum a b = reverse $ binarySum' (reverse a) (reverse b) 0
  where 
      binarySum' :: [Int] -> [Int] -> Int -> [Int]
      binarySum' []     []     0 = []
      binarySum' []     []     1 = [1]
      binarySum' [] b carry = binarySum' (replicate (length b) 0) b carry
      binarySum' a [] carry = binarySum' (replicate (length a) 0) a carry
      binarySum' (a:as) (b:bs) carry = resSum : binarySum' as bs resCarry
          where 
              (resSum, resCarry) = binarySum'' a b carry
      binarySum'' :: Int -> Int -> Int -> (Int,Int)
      binarySum'' x y z = if sum > 1 then (sum - 2, 1) else (sum, 0)
        where
            sum = x + y + z

binSum :: [Int] -> [Int] -> [Int]
binSum a b
  | sum a == 0          = dropWhile (==0) b
  | sum b == 0          = dropWhile (==0) a
  | length a < length b = binSum' (replicate (length b - length a) 0 ++ a) b
  | length b < length a = binSum' (replicate (length a - length b) 0 ++ b) a
  | otherwise           = binSum' a b 
    where 
        binSum' :: [Int] -> [Int] -> [Int]
        binSum' a b = binSum (carries ++ [0]) sums
            where 
                (carries, sums) = (zipWith and' a b, zipWith xor a b)

and' :: Int -> Int -> Int 
and' 1 1 = 1
and' _ _ = 0

xor :: Int -> Int -> Int 
xor a b = if a + b == 1 then 1 else 0
       
------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
test :: BinHeap Char
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]

test = [Node 'i' 0 [],Node 'i' 1 [Node 'm' 0 []],Node 'l' 2 [Node 'n' 1 [Node 'o' 0 []],Node 'p' 0 []]]

