chop :: Int -> (Int, Int)
chop n
  | n < 10 = (0,n)
  | otherwise = (q + 1, r)
      where (q,r) = chop (n - 10)

addDigit :: Int -> Int -> Int
addDigit i d = 10 * i + d

concatenate :: Int -> Int -> Int
concatenate _ 0 = 0
concatenate 0 _ = 0
concatenate m n = addDigit (concatenate m q) r
    where(q, r) = chop n

fib :: Int -> Int 
fib 0 = 0 
fib 1 = 1
fib n = fib' 0 1 (n - 1)
  where 
      fib' :: Int -> Int -> Int -> Int
      fib' _ res 0 = res
      fib' f s c = fib' s (f + s) (c - 1)

{-     

goldenRatio :: Float -> Float 
goldenRatio e = goldenRatio' 1 2 1
  where goldenRatio' f f' r 
    | abs ((r-r')/r) < e = r'
    | otherwise          = gr f' (f + f') f'
      where r' = fromIntegral f' / fromIntegral f 
-}