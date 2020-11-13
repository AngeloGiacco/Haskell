import Data.Char

depunctuate :: String -> String
depunctuate []= []
depunctuate x = filter (\l -> not $ elem l ";.,:") x

makeString :: [Int] -> String
makeString [] = []
makeString ns = map chr ns

enpower :: [Int] -> Int
enpower n = foldr1 (\x y -> y ^ x) n

revAll :: [[a]] -> [a]
revAll []= []
revAll x = concat $ map reverse x

rev :: [a] -> [a]
rev xs 
  = foldl (\x y -> y : x ) [] xs

dezip :: [(a,b)] -> ([a],[b])
dezip lst = foldr (\x y -> (fst x : fst y,snd x : snd y) ) ([],[]) lst

allSame :: [Int] -> Bool
allSame (n:ns) = and $ map (== n) ns 

allSame' :: [Int] -> Bool
allSame' n@(_:ns) = and $ zipWith (==) n ns

fact :: Float -> [Float]
--Pre: n >= 2
fact n = scanl (*) 1.0 [1.0..n] 

e :: Float 
e = sum $ map (1/) (fact 100)

fibs :: [Int] 
fibs = 1 : scanl (+) 1 fibs

squash :: (a->a->b) -> [a] -> [b]
squash f (x : y : []) = [f x y]
squash f (fst:snd:xs) = f fst snd : squash f (snd:xs)

squash' :: (a->a->b) -> [a] -> [b]
squash' f lst@(_:xs) = zipWith f lst xs

converge ::  (a -> a -> Bool) -> [a] -> a 
converge _ [x] = x
converge f (x:y:xs) = if f x y then x else converge f (y:xs)

e5dp :: Float
e5dp = converge lim (scanl (+) 0 (map (1/) facts))
  where facts   = scanl (*) 1 [1..] 
        lim x y = abs (x - y) < 0.000001

limit ::  (a -> a -> Bool) -> [a] -> [a]
limit f (x:y:xs) 
  | f x y     = x : limit f (y:xs)
  | otherwise = []

repeatUntil :: (a -> Bool) -> (a -> a) -> a ->a
repeatUntil cond f = head . filter cond . iterate f

any', all' ::  (a -> Bool) -> [a] -> Bool
any' p = or . map p 
all' p = and . map p

isElem :: Eq a => a -> [a] -> Bool
isElem = any . (==)

infixl 9 <.>
(<.>) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
f <.> g = fg 
  where fg x y = f (g x y)

pipeline :: [a -> a] -> [a] -> [a]
pipeline = map . foldr (.) id