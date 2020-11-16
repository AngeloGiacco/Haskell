import Data.Char
import Data.List

precedes :: String -> String -> Bool
precedes _ ""  = False 
precedes "" _  = True

precedes (a:as) (b:bs)
  | a < b     = True 
  | a > b     = False 
  | otherwise = precedes as bs

pos :: [Int] -> Int -> Int
pos lst target = pos' lst 0 
  where 
    pos' :: [Int] -> Int -> Int
    pos' (x:xs) counter
      | x == target = counter
      | otherwise   = pos' xs (counter + 1)

twoSame :: [Int] -> Bool
twoSame []     = False
twoSame [_]    = False 
twoSame (x:xs) = checkDuplicate x xs || twoSame xs
  where checkDuplicate :: Int -> [Int] -> Bool
        checkDuplicate target (y:ys) 
          | target == y = True
          | null ys     = False
          | otherwise   = checkDuplicate target ys

rev :: String -> String
rev "" = "" 
rev (x:xs) = concat(rev xs : [[x]])

substring :: String -> String -> Bool 
substring str (x:xs)
  | x : take (length str - 1) xs == str = True
  | length xs < length str              = False 
  | otherwise                           = substring str xs

transpose :: String -> String -> String -> String 
transpose original anagram shiftedAnagram = drop n original ++ take n original 
  where n = length original - (pos (map ord shiftedAnagram) (ord (anagram !! 0)))

removeWhitespace :: String -> String 
removeWhitespace "" = ""
removeWhitespace (x:xs)
  | elem x whitespace = input'
  | otherwise         = x : input' 
    where input' = removeWhitespace xs
          whitespace = [' ','\t','\n']

nextWord :: String -> (String,String)
nextWord s = (take len s, drop len s)
  where 
    noWhite = removeWhitespace s
    len = checkWordLength s noWhite
checkWordLength :: String -> String -> Int 
checkWordLength orig noW = checkWordLength' orig noW 0
  where 
    checkWordLength' (x:xs) (y:ys) counter 
      | x == y    = checkWordLength' xs ys (counter + 1)
      | otherwise = counter
    checkWordLength' [] [] counter = counter

splitUp :: String -> [String]
splitUp s = [x] ++ splitUp rest
  where (x,xs) = nextWord s
        rest = drop (findRest xs (removeWhitespace xs)) xs 
findRest :: String -> String -> Int 
findRest restSpaced restStripped = findRest' restSpaced restStripped 0
  where 
    findRest' :: [Char] -> [Char] -> Int -> Int
    findRest' x "" counter = length x + counter
    findRest' (x:xs) (y:ys) counter 
      | x /= y    = findRest' xs (y:ys) (counter + 1)
      | otherwise = counter

maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit [_] = 0
maxProfit (x:xs) = if todayProfit > tomorrowProfit then todayProfit else tomorrowProfit 
  where todayProfit = max' xs - x
        tomorrowProfit = maxProfit xs

        max' :: [Int] -> Int 
        max' [y] = y
        max' (y : ys) = max y (max' ys)

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2 []
  where 
    primeFactors' :: Int -> Int -> [Int] -> [Int]
    primeFactors' n cur lst 
      | n == cur       = n : lst
      | rem n cur == 0 = primeFactors' (div n factor) cur lst ++ primeFactors' factor cur lst
      | otherwise      = primeFactors' n (cur + 1) lst
        where factor = div n cur

hcf :: Int -> Int -> Int 
hcf x y = product (primeFactors x) (primeFactors y)
  where
    product :: [Int] -> [Int] -> Int 
    product a b = foldr (*) 1 (a\\(a\\b))

lcm :: Int -> Int -> Int 
lcm x y = product' (primeFactors x) (primeFactors y)
  where
    product' :: [Int] -> [Int] -> Int 
    product' a b = foldr (*) 1 (b++(a\\b))