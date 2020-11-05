-- Problem 1
-- find the last element of a list 

import Data.List

myLast :: [a] -> a 
myLast [x] = x 
myLast (_:xs) = myLast xs
myLast [] = error "can not take an empty list"

myLast' :: [a] -> a
myLast' = head . reverse

myLast'' :: [a] -> a
myLast'' = last

-- Problem 2 
-- find the element of a list 

myPenultimate :: [a] -> a 
myPenultimate [x,_] = x
myPenultimate (_:xs)       = myPenultimate xs 

myPen' :: [a] -> a
myPen' = head . tail . reverse

-- Problem 3
-- find the kth element of a list
-- the first element has index one 

elementAt :: [a] -> Int -> a 
elementAt (x:_) 1  = x 
elementAt [] _     = error "Index out of bounds"
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
-- find the number of elements in a list 

len :: [a] -> Int 
len x = len' x 0
  where 
      len' :: [a] -> Int -> Int
      len' [] n     = n
      len' (_:xs) n = len' xs (n+1)

len' :: [a] -> Int 
len' = length

len'' :: [a] -> Int
len'' [] = 0 
len''(_:xs) = 1 + len'' xs

-- Problem 5
-- Reverse a list. 
rev :: [a] -> [a] 
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [a] -> [a] 
rev' =  foldl (flip (:)) []

rev'' :: [a] -> [a] -- didn't come up with this but thought it looked it gd
rev'' list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

-- Problem 6
-- Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x). 

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a] 

flatten :: NestedList a -> [a]
flatten x = flt' x []
  where flt' (Elem e)      acc = e:acc
        flt' (List (l:ls)) acc = flt' l (flt' (List ls) acc)
        flt' (List [])     acc = acc

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. 
-- The order of the elements should not be changed. 
-- compress [1,1,1,2,2] => [1,2]

compress :: (Eq a) => [a] -> [a] 
compress x = compress' x [] 
  where 
    compress' :: (Eq a) => [a] -> [a] -> [a] 
    compress' [] res  = res 
    compress' [x] res = x : res
    compress' (x:x':xs) res 
      | x == x'   = compress' (x':xs) res 
      | otherwise = x : compress' (x':xs) res

-- another nice solution found online
compress'' :: Eq a => [a] -> [a]
compress'' []     = []
compress'' (x:xs) = x : compress'' (dropWhile (== x) xs)

-- and very nice as well
comp :: Eq a => [a] -> [a]
comp = map head . group

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists. 


-- a trivial solution
pack :: Eq a => [a] -> [[a]]
pack = group

-- actual solution

pack' :: Eq a => [a] -> [[a]]
pack' xs = pack'' xs [] []
  where 
    pack'' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
    pack'' [] acc res = reverse (acc : res) 
    pack'' (x:xs) acc res
      | null acc     = pack'' xs (x:acc) res
      | x == head acc = pack'' xs (x:acc) res 
      | otherwise     = pack'' xs [x] (acc:res)

pack'' :: (Eq a) => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x : takeWhile (==x) xs) : pack'' (dropWhile (==x) xs)

-- Problem 10
-- Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 

encode :: (Eq a) => [a] -> [(Int,a)] 
encode = map (\x -> (length x, head x)) . pack''

