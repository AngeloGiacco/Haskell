data MultiTree a = Node a [MultiTree a]
        deriving (Eq, Show)

nodes :: MultiTree a -> Int 
nodes (Node _ []) = 1
nodes (Node _ lst) = sum (map nodes lst) + 1

ipl :: MultiTree a -> Int
ipl = ipl' 0 
  where 
    ipl' :: Int -> MultiTree a -> Int  
    ipl' n (Node _ []) = n 
    ipl' n (Node _ xs) = sum (map (ipl' (n + 1)) xs) + n

bottomUp :: MultiTree a -> [a]
bottomUp (Node x []) = [x]
bottomUp (Node x xs) = concatMap bottomUp xs ++ [x]