data Stream a = a :< Stream a 
infix 3 :< 

repeat' :: a -> Stream a 
repeat' x = xs where xs = x :< repeat' x 

list :: Int -> Stream a -> [a]
list 0 _ = [] 
list n (x:<xs) = x : list (n-1) xs

instance Show a => Show (Stream a) where 
    show xs = show (list 10 xs)

iterate' :: (a->a) -> a -> Stream a 
iterate' f x = x :< iterate' f (f x)

nats :: Num a => Stream a 
nats = iterate' (+1) 0