import Data.List

-- Problem 31
-- Determine whether a given integer number is prime 
isPrime :: Int -> Bool
isPrime n = not $ any (\x -> rem n x == 0) [2..lim]
  where 
      num = fromIntegral n :: Float
      lim = round $ sqrt num

-- Problem 32
-- GCD using euclidean algorithm
gcd' :: Int -> Int -> Int 
gcd' x y
  | x > y     = gcd' (x-y) y
  | y > x     = gcd' x (y-x)
  | otherwise = x
  
-- Problem 33
-- (*) Determine whether two positive integer numbers are coprime. 
-- Two numbers are coprime if their greatest common divisor equals 1. 

isCoprime :: Int -> Int -> Bool
isCoprime x y = gcd' x y == 1

-- Problem 34 
-- Calculate Euler's totient function phi(m). 
-- Euler's so-called totient function phi(m) is 
-- defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 

phi :: Int -> Int
phi 1 = 1
phi n = length $ filter (`isCoprime` n) [1..n]

-- Problem 35
primeFactors :: Int -> [Int] 
primeFactors n = filter (\x -> rem n x == 0 && isPrime x) [2..n]

-- Problem 36
-- primeFactors with multiplicity
repeatedPrimeFactors :: Int -> [Int]
repeatedPrimeFactors 1 = []
repeatedPrimeFactors n = x : repeatedPrimeFactors (div n x)
  where 
      (x:_) = primeFactors n

primeFactorsMultiplicity :: Int -> [(Int,Int)]
primeFactorsMultiplicity = map (\lst@(x:_) -> (x,length lst)) . group . repeatedPrimeFactors 

-- Problem 37
phi' :: Int -> Int 
phi' n = product $ map (\(n,m) -> ((n - 1) * n) ^ (m - 1) ) (primeFactorsMultiplicity n)

-- Problem 39
primeRange :: Int -> Int -> [Int]
primeRange start end = [n | n <- [start..end], isPrime n]

-- Problem 40
goldbach :: Int -> (Int,Int) 
goldbach n = head [(x,y) | x <- primes, y <- primes, x+y == n ]
  where 
      primes = primeRange 2 (n-2)

-- Problem 41 
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList start end = map goldbach [start',(start'+2)..end]
    where start' = if even start then start else start + 1

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' start end min = filter (\(x,_) -> x > min) (goldbachList start end)