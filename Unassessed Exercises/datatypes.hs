type Vertex = (Float,Float)
data Shape = Triangle Float Float Float 
           | Square Float
           | Circle Float
           | Polygon [Vertex] 
           deriving (Eq, Show)

area :: Shape -> Float 
area (Triangle a b c) = sqrt mult
  where mult = foldr (*) 1.0 [s,(s-a),(s-b),(s-c)]
        s = (a + b + c) / 2.0
area (Square side) = side ^ 2
area (Circle r) = pi * r ^ 2
area (Polygon (x : y : z : [])) = areaTriangle x y z 
area (Polygon (x:y:z:vs)) = areaTriangle x y z  + area (Polygon (x:z:vs))

areaTriangle :: Vertex -> Vertex -> Vertex -> Float 
areaTriangle x y z =  area (Triangle a b c)
  where a = sqrt ((fst y - fst x)^2 + (snd y - snd x)^2)
        b = sqrt ((fst z - fst x)^2 + (snd z - snd x)^2)
        c = sqrt ((fst z - fst y)^2 + (snd z - snd y)^2) 

type Date = (Int, Int, Int)
age :: Date -> Date -> Int
age (date, month, yr) (date', month', yr')
  | (month, date) <= (month', date') = yr' - yr
  | otherwise                        = yr' - yr - 1

data Tree a = Empty | Node (Tree a ) a (Tree a)

flatten :: Tree a -> [a]
flatten t = 
  flatten' t []
    where
      flatten' Empty xs= xs 
      flatten' (Node l x r) xs 
        = flatten' l (x : flatten' r xs)

data Tree' = Leaf | Node' Tree' Tree'
              deriving (Eq, Show)

makeTrees ::  Int -> [Tree']
makeTrees 0 = [Leaf]
makeTrees n = [Node' l r | (t, t') <- map makeTrees' [0..n-1],l <- t, r <- t']
  where
    makeTrees' k = (makeTrees k, makeTrees (n - k - 1))

    
