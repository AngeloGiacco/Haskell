import Data.List

-- Problem 21
-- Insert an element at a given position into a list. 
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs n = take (n-1) xs ++ (e : drop (n-1) xs)

-- Problem 22
-- Create a list containing all integers within a given range. 
range :: Int -> Int -> [Int] 
range n end 
  | n == end  = [n]
  | otherwise = n : range (n+1) end

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
{-
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]
-}

-- Problems requiring random number generation are skipped whilst I 
-- can't do the import necessary for random number generation

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

perms :: Int -> [a] -> [[a]]
perms n = take n . permutations

perm :: Eq a => [a] -> [[a]] -- "abc" -> ["abc","acb","bca","bac"...]
perm [x]   = [[x]]
perm [x,y] = [[x,y],[y,x]]
perm xs     = concatMap (\x -> map (x:) (perm (remove x xs []))) xs
  where 
    remove :: Eq a => a -> [a] -> [a] -> [a] 
    remove t (x:xs) lst
      | x == t    = lst ++ xs 
      | otherwise = remove t xs (lst ++ [x])

permAllowingRepetition :: Eq a => [a] -> [[a]]
permAllowingRepetition xs = permHelper xs (length xs)

permHelper :: Eq a => [a] -> Int -> [[a]]
permHelper _ 0  = []
permHelper xs 1 = map (:[]) xs
permHelper xs n = concatMap (\x -> map (x:) (permHelper xs (n-1))) xs

-- Problem 27
-- In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
-- Write a function that generates all the possibilities and returns them in a list. 
group3 :: Eq a => [a] -> [([a], [a], [a])]
group3 xs = concatMap (\x -> concatMap (\y -> map (\z -> (x,y,z)) (combinations 4 ((xs \\ y) \\ x))) (combinations 3 (xs \\ x))) (combinations 2 xs)

len :: Int
len = (length . group3) [1..9]