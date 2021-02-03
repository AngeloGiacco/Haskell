data Code = Lf Int | Nd 
  deriving Show
data Tree = Node Tree Tree | Leaf Int
  deriving Show

enc :: Tree -> [Code]
enc (Leaf i) = [ Lf i ]
enc (Node t1 t2) = Nd:enc t1 ++ enc t2

dec :: [Code] -> Tree
dec cds = t
  where
    (t, []) = decAux cds

decAux :: [Code] -> (Tree, [Code])
decAux  ((Lf i):cds) = ( Leaf i,  cds )
decAux  (Nd:cds)   = ( Node t1 t2, cds2)
  where
      (t1, cds1) = decAux cds
      (t2, cds2) = decAux cds1

encd :: Tree -> [Code]
encd (Leaf i) = [ Lf i ]
encd (Node t1 t2) = (encd t1)++(encd t2)++ [Nd]

decd :: [Code] -> Tree
decd cds = decdAux cds []

decdAux :: [Code] -> [Tree] -> Tree
decdAux [] (t:ts)  =  t
decdAux ((Lf i):cds) ts  =  decdAux cds  ((Leaf i):ts)
decdAux (Nd:cds) (t1:t2:ts)  =  decdAux cds  ((Node t2 t1):ts)