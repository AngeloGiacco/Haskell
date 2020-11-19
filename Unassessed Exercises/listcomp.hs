import Data.List 

findAll :: Int -> [(Int,Int)] -> [Int]
findAll val t = [y | (x, y) <- t, x == val]

product' :: [Int] -> Int
product' = foldr (*) 1

remove :: Int -> [(Int,Int)] -> [(Int,Int)]
remove val t = [(x,y) | (x, y) <- t, x /= val]

remove' :: Int -> [(Int,Int)] -> [(Int,Int)]
remove' val t =  filter notVal t
  where
      notVal :: (Int,Int) -> Bool
      notVal (x,_) = x /= val

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort x = l ++ g
  where 
      sublist = genSublist x
      l = quickSort(fst sublist)
      g = quickSort(snd sublist) 

genSublist :: [Int] -> ([Int],[Int])
genSublist [] = ([],[])
genSublist [x] = ([x],[])
genSublist (x:xs) = (lower++[x],greater) 
  where greater = [num | num <- xs, num >= x]
        lower = xs \\ greater

allSplits :: [a] -> [([a], [a])]
allSplits x = split' 1 x
  where 
    split' n y
      | n == lim  = [splitAt n y] 
      | otherwise = splitAt n y : split' (n+1) y
        where 
          lim = length y - 1

prefixes :: [t] -> [[t]]
prefixes [z] = [[z]]
prefixes (x:xs) = [x] : map (x:) (prefixes xs)

substrings :: [t] -> [[t]]
substrings [] = []
substrings x'@(_:xs) = prefixes x' ++ substrings xs

perms :: Eq t => [t] -> [[t]]
perms [] = []
perms [x] = [[x]]
perms lst = [x : xs | x <- lst, xs <- perms (lst\\[x])]

routes :: Int -> Int -> [(Int,Int)] -> [[Int]]
routes start end edges = routes' start end edges [] []

--Extract the next possible edges
routes' :: Int -> Int -> [(Int,Int)] -> [Int] -> [[Int]] -> [[Int]]
routes' current end edges currentRoute routeList 
  | current == end = (currentRoute ++ [current]) : routeList 
  | nextEdge == [] = routeList
  | otherwise      = routes'' current end remainingEdges (values nextEdge) currentRoute routeList
    where
      nextEdge = lookUp current edges 
      remainingEdges = edges \\ nextEdge

--Updates the parameters
-- current node -> target node -> list of edges -> candidate for next node ->
-- currentRoute -> routeList 
routes'' :: Int -> Int -> [(Int,Int)] -> [Int] -> [Int] -> [[Int]] -> [[Int]] 
routes'' _ _ _ [] _ routeList 
  = routeList
routes'' current end edges (c:cs) currentRoute routeList
  = routes' c end edges (currentRoute ++ [current]) routeList 
      ++ routes'' current end edges cs currentRoute routeList

lookUp :: Int -> [(Int,Int)] -> [(Int,Int)]
lookUp key edges = [(from,to) | (from,to) <- edges, from == key]

values :: [(Int,Int)] -> [Int]
values [] = []
values ((_,to):es) = to : values es

values' :: [(Int,Int)] -> [Int]
values' edges = [to | (_,to) <- edges ]