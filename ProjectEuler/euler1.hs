-- Problem 1

sumMults3or5 :: Int 
sumMults3or5 = sum [n | n <- [1..1000] , n `mod` 5 == 0 || n `mod` 3 == 0]

-- Problem 2

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sum' :: Int 
sum' = sum $ filter even (takeWhile (<4000000) fibs) 

-- Problem 3
isPrime :: Int -> Bool 
isPrime 1 = False
isPrime n = and [n `rem` x /= 0 | x <- [2..n-1]]

primeFactors :: Int -> [Int] 
primeFactors n 
  | isPrime n = [n]
  | otherwise = pF : primeFactors (fst (quotRem n pF))
      where 
          (pF:_) = filter (\x -> n `rem` x == 0) primes
          primes = filter isPrime [1..n]

maximumPF :: Int -> Int --call maximumPF 600851475143
maximumPF = maximum . primeFactors
