-- Code required from problems 1-10

pack'' :: (Eq a) => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x : takeWhile (==x) xs) : pack'' (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int,a)] 
encode = map (\x -> (length x, head x)) . pack''

-- Problem 11 
-- Modify the result of problem 10 in such a way that if an element has no duplicates 
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 

-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data ListItem a = Multiple Int a | Single a 
  deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encode' . encode
  where 
      encode' :: (Int,a) -> ListItem a 
      encode' (1,x) = Single x 
      encode' (n,x) = Multiple n x

-- Problem 12 
-- Create a decoder for encode modified

decodeModified :: [ListItem a] -> [a] 
decodeModified = concatMap (repeater . decomposer)
  where 
    decomposer :: ListItem a -> (Int,a)
    decomposer (Single x) = (1,x)
    decomposer (Multiple n x) = (n,x)

    repeater :: (Int,a) -> [a]
    repeater (x,y) = replicate x y 

-- Problem 13
-- do encoding directly 

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys


directEncode :: Eq a => [a] -> [ListItem a]
directEncode = map encode'' . encode'
  where 
      encode'' :: (Int,a) -> ListItem a 
      encode'' (1,x) = Single x 
      encode'' (n,x) = Multiple n x

-- Problem 14 
-- Duplicate a list 
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x: dupli xs


-- Problem 15
-- repeat any n times
repeater :: Int -> [a] -> [a]
repeater n = concatMap $ replicate n

-- Problem 16 
-- drop every nth element 
dropEveryN :: Int -> [a] -> [a]
dropEveryN n xs
  | length xs < n = xs
  | otherwise     = take (n-1) xs ++ dropEveryN n (drop n xs)

--Problem 17
split :: Int -> [a] -> ([a],[a])
split n xs = split' n xs []
  where 
      --split' takes index value, original array, built up array
      split' :: Int -> [a] -> [a] -> ([a],[a])
      split' 0 rest acc = (acc,rest)
      split' n (x:xs) acc = split' (n-1) xs (acc ++ [x])

-- Problem 18
-- Given two indices, i and k, the slice is the list containing the elements 
-- between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1. 
slice :: [a] -> Int -> Int -> [a]
slice xs start end = (fst . split (end - start + 1) . snd . split (start-1)) xs

slice' :: [a] -> Int -> Int -> [a]
slice' xs start end = (take (end - start + 1) . drop (start - 1)) xs

-- a nice solution from online
slice'' :: [a] -> Int -> Int -> [a]
slice'' xs a b = map fst . filter ((>=a) . snd) $ zip xs [1..b]

-- Problem 19
-- Rotate a list N places to the left. 
rotate :: [a] -> Int -> [a]
rotate x n
  | n >= 0 = drop n x ++ take n x 
  | otherwise = drop (length x + n) x ++ take (length x + n) x


-- Problem 20
-- remove the kth element from a list
removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = removeAt' n xs [] 
  where 
      removeAt' :: Int -> [a] -> [a] -> (a,[a])
      removeAt' _ [] _ = error "ran out of space"
      removeAt' 1 (x:xs) acc = (x,acc++xs)
      removeAt' n (x:xs) acc = removeAt' (n-1) xs (x:acc)


