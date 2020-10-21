type Matrix = [[Float]]


rref :: Matrix -> Matrix 
rref [x@[x1,x2,x3,_],y@[y1,y2,y3,_],z@[z1,z2,z3,_]]
  | x1 /= 1 && x1 /= 0   = rref [map (/ x1) x, y, z]
  | y1 /= 0 && x1 /= 0   = rref [x, zipWith (\a b -> a - (b * y1)) y x,z]
  | z1 /= 0 && x1 /= 0   = rref [x, y , zipWith (\a b -> a - (b * z1)) z x]
  | y2 /= 1 && y2 /= 0   = rref [x, map (/ y2) y, z]
  | z2 /= 0 && y2 /= 0   = rref [x, y, zipWith (\a b -> a - (b * z2)) z y]
  | z3 /= 1 && z3 /= 0   = rref [x, y, map (/ z3) z]
  | y3 /= 0 && z3 /= 0   = rref [x, zipWith (\a b -> a - (b * z3)) z y, z]
  | x3 /= 0 && z3 /= 0   = rref [zipWith (\a b -> a - (b * z3)) z x, y, z]
  | x2 /= 0 && y2 /= 0   = rref [zipWith (\a b -> a - (b * y2)) y x, y, z]
  | otherwise = [x,y,z]

rank :: Matrix -> Float 
rank m = countOnes $ rref m 
  where
      countOnes :: Matrix -> Float
      countOnes [[x1,_,_,_],[_,y2,_,_],[_,_,z3,_]]
        = x1 + y2 + z3