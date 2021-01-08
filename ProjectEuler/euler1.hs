-- Problem 1

sumMults3or5 :: Int 
sumMults3or5 = sum [n | n <- [1..1000] , n `mod` 5 == 0 || n `mod` 3 == 0]


