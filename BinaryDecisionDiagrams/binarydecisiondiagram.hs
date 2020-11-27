import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x 
  = snd . head . filter (\(y,_) -> y == x)

checkSat :: BDD -> Env -> Bool
checkSat (id,nodes) envs
 | nextNode `elem` [1,0] = nextNode == 1
 | otherwise             = checkSat (nextNode, nodes) envs
  where 
    node@(f,_,_) = lookUp id nodes
    bool         = lookUp f envs
    nextNode     = getNode node bool

getNode :: (Index, NodeId, NodeId) -> Bool -> NodeId
getNode (_,y,_) False = y
getNode (_,_,z) True  = z 

sat :: BDD -> [[(Index, Bool)]]
sat (id,nodes)
  | id == 0   = [] 
  | id == 1   = [[]]
  | otherwise =  l' ++ r'
  where 
    (index,l,r) = lookUp id nodes 
    r'      = map (\x -> (index,True): x) $ sat (r,nodes)
    l'      = map (\x -> (index,False): x) $ sat (l,nodes)


------------------------------------------------------
-- PART II

simp :: BExp -> BExp 
simp (Or (Prim False) (Prim False)) = Prim False 
simp (Or (Prim _) (Prim _))         = Prim True      
simp (And (Prim True) (Prim True))  = Prim True
simp (And (Prim _) (Prim _))        = Prim False 
simp (Not (Prim False))             = Prim True
simp (Not (Prim True))              = Prim False 
simp x                              = x

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef x) ind b
  | ind == x              = Prim b 
  | otherwise             = IdRef x 
restrict x@(Prim _) ind b = x
restrict (And x y) ind b  = simp $ And (restrict x ind b) (restrict y ind b)
restrict (Or x y) ind b   = simp $ Or (restrict x ind b) (restrict y ind b)
restrict (Not x) ind b    = simp $ Not (restrict x ind b)

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e inds
  = (2,buildBDD' e 2 inds) 

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> [BDDNode]
buildBDD' e id [x]           = [(id,(x,l,r))]
  where 
    l = if removePrim $ restrict e x False then 1 else 0 
    r = if removePrim $ restrict e x True then 1 else 0 
    removePrim :: BExp -> Bool
    removePrim (Prim x) = x

buildBDD' e id (ind:indexes) = (id, (ind, leftID,rightID)):l ++ r
  where 
    leftID = 2 * id 
    rightID = 2 * id + 1
    l = buildBDD' (restrict e ind False) leftID indexes
    r = buildBDD' (restrict e ind True) rightID indexes

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD exp is = (elimSameSubsNodes . removeDuplicates) $ buildBDD exp is

-- removes node if subtrees are equal
elimSameSubsNodes :: BDD -> BDD
elimSameSubsNodes bdd@(i,xs) = (i,res)
  where 
    sharedSubNodes = filter (\(_,(_,x,y)) -> x == y) xs
    removed = removeFrom xs sharedSubNodes
    res = replace removed sharedSubNodes

    removeFrom :: [BDDNode] -> [BDDNode] -> [BDDNode]
    removeFrom bdd ts = bdd \\ ts

    replace :: [BDDNode] -> [BDDNode] -> [BDDNode] 
    replace [] _      = []
    replace (n:ns) ts = replace' n ts : replace ns ts
      where
        replace' :: BDDNode -> [BDDNode] -> BDDNode
        replace' (id,(ind,l,r)) s = (id,(ind,l',r'))
          where
            subKeys       = map fst subKeysValMap
            subKeysValMap = map (\(x,(_,val,_)) -> (x,val)) s   
            l' = if l `elem` subKeys then lookUp l subKeysValMap else l 
            r' = if r `elem` subKeys then lookUp r subKeysValMap else r

findDuplicates :: [BDDNode] -> [(Int,Int)]
findDuplicates bdd = findDuplicates' bdd []
  where 
    findDuplicates' :: [BDDNode] -> [(Int,Int)] -> [(Int,Int)]
    findDuplicates' [x] acc = acc
    findDuplicates' ((n,(x,y,z)):ns) acc
      | null dups = findDuplicates' ns acc
      | otherwise = findDuplicates' newNs newAcc 
      where 
        newAcc    = map (\(id,_) -> (id,n)) dups ++ acc
        newNs     = ns \\ dups
        dups      = filter (\(n',(x',y',z')) -> n /= n' && x == x' && y == y' && z == z') ns

replaceDuplicates :: [BDDNode] -> [(Int,Int)] -> [BDDNode]
replaceDuplicates ns removeMap = replace' dupsRemoved removeMap idsToRemove
  where 
    idsToRemove = map fst removeMap
    dupsRemoved = filter (\(id,_) ->  id `notElem` idsToRemove) ns
    replace' :: [BDDNode] -> [(Int,Int)] -> [Int] -> [BDDNode]
    replace' [] _ _                             = []
    replace' ((id,(ind, l,r)):ns) remMap remIds = (id,(ind, l',r')) : replace' ns remMap remIds
      where   
        l' = if l `elem` remIds then lookUp l remMap else l 
        r' = if r `elem` remIds then lookUp r remMap else r

removeDuplicates :: BDD -> BDD
removeDuplicates (i,ns) = (i,replaceDuplicates ns (findDuplicates ns))
    

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


