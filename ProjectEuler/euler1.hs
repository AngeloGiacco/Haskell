-- Problem 1

sumMults3or5 :: Int 
sumMults3or5 = sum [n | n <- [1..1000] , n `mod` 5 == 0 || n `mod` 3 == 0]

--Problem 2

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sum' :: Int 
sum' = sum $ filter even (takeWhile (<4000000) fibs) 
