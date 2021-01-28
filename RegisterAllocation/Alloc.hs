module Alloc where

import Data.Maybe
import Data.List
import Data.Function

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count target 
  = length . filter (==target)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (n:ns,lst) 
  = (n, deg) : degrees (ns,lst)
  where 
    deg = (length . filter (\(x,y) -> x == n || y == n)) lst
degrees ([],_)     = []

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_,es) 
  = [if x == n then y else x | (x,y) <- es, x == n || y == n]
 
removeNode :: Eq a => a -> Graph a -> Graph a
removeNode node (ns, es) 
  = (ns \\ [node], es')
    where 
      es' = [e | e@(x,y) <- es, x /= node && y /= node]


------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([],_) 
  = []
colourGraph cMax g@(nodes,es) 
  = (node,c) : cMap
  where 
    degs@((_,mindeg):_) = sortBy (compare `on` snd) (degrees g)
    minDegNodes         = takeWhile ((==mindeg) . snd) degs
    (node,_):_          = sortBy (compare `on` fst) minDegNodes
    g'                  = removeNode node g
    cMap                = colourGraph cMax g'
    neighbourLst        = neighbours node g
    occupied            = map (`lookUp` cMap) neighbourLst
    available           = [1..cMax] \\ occupied
    c                   = if null available then 0 else head available



------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap ids
  = ("return","return") : map idToVar ids
  where 
    idToVar :: (Id,Colour) -> (Id,Id)
    idToVar (id,0) = (id,id)
    idToVar (id,n) = (id, "R"++ show n)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments ids idMap 
  =  foldr idToStatement [] ids 
  where 
    idToStatement :: Id -> [Statement] -> [Statement]
    idToStatement id stats 
      | isNothing assignment = stats
      | otherwise            = Assign register (Var id) : stats
        where 
          assignment = lookup id idMap
          register   = fromJust assignment

renameExp :: Exp -> IdMap -> Exp
renameExp (Var v) idMap         
  = Var (lookUp v idMap)
renameExp (Apply op e1 e2) idMap 
  =  Apply op (renameExp e1 idMap) (renameExp e2 idMap)
renameExp c@(Const _) _    
  = c

renameBlock :: Block -> IdMap -> Block
renameBlock b idMap = removeSelfAssignments (renameBlock' b idMap)
  where  
    renameBlock' :: Block -> IdMap -> Block 
    renameBlock' (Assign id e:ss) idMap 
      = Assign id' (renameExp e idMap) : renameBlock ss idMap 
        where 
          Var id' = renameExp (Var id) idMap 
    renameBlock' (If p tB fB:ss) idMap
      = if' : renameBlock ss idMap 
        where 
          if' = If (renameExp p idMap) tB' fB'
          tB' = renameBlock tB idMap
          fB' = renameBlock fB idMap
    renameBlock' (While p b:ss) idMap 
      = While (renameExp p idMap) (renameBlock b idMap) :  renameBlock ss idMap
    renameBlock' [] _ 
      = []
    removeSelfAssignments :: Block -> Block 
    removeSelfAssignments (a@(Assign id e):ss)
       | e == Var id = removeSelfAssignments ss
       | otherwise   = a : removeSelfAssignments ss
    removeSelfAssignments (If p tB fB:ss)
      = If p (removeSelfAssignments tB) (removeSelfAssignments fB) : ss 
    removeSelfAssignments (While p b:ss)
      = While p (removeSelfAssignments b) : ss 
    removeSelfAssignments []
      = []


renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)
-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG ids = (elems, edges)
  where
    elems = (nub . concat) ids 
    edges = getEdges elems 
    getEdges :: [Id] -> [Edge Id] 
    getEdges (i:is) = checkLive i is ++ getEdges is
    getEdges []     = []
    checkLive :: Id -> [Id] -> [Edge Id] 
    checkLive i (i':is)
      | any (\x -> i `elem` x && i' `elem` x) ids = (i,i') : checkLive i is 
      | otherwise                                 = checkLive i is
    checkLive _ [] = []

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

-- MY VERY INCOMPLETE ATTEMPT

buildCFG :: Function -> CFG
buildCFG (_,_,b) = blockToCFG b 

blockToCFG :: Block -> CFG 
blockToCFG = blockToCFG' 0

blockToCFG' :: Int -> Block -> CFG
-- unfortunately i didn't get time to 
blockToCFG' n (If p tB fB :ss) 
  = ((("_", usedVars p), [n+1,falsePointer]) : trueCFG) ++ falseCFG
    where 
      trueCFG      = blockToCFG' (n + 1) tB 
      falsePointer = succ' n  (concatMap snd trueCFG)
      falseCFG     = blockToCFG' falsePointer fB
blockToCFG' n (While p b :ss) 
  = res ++ blockToCFG' (maxN + 1) ss 
    where 
      res  = whileToCFG n (While p b)
      maxN = succ' n (concatMap snd res)
blockToCFG' n (Assign id e :ss) 
  | id == "return" = [(("return", usedVars e), [])]
  | otherwise      = [((id,usedVars e), [n+1])]
blockToCFG' _ []
  = []

usedVars :: Exp -> [String]
usedVars = nub . usedVars'
  where 
    usedVars' :: Exp -> [String] 
    usedVars' (Var id)        = [id] 
    usedVars' (Const _)       = []
    usedVars' (Apply o e1 e2) = usedVars' e1 ++ usedVars' e2

succ' :: Int -> [Int] -> Int
-- used to find the next succ point 
succ' n [] = n + 1 
succ' n ns = maximum ns + 1

whileToCFG :: Int -> Statement -> CFG 
whileToCFG n (While p b)
  = (("_", usedVars p), [n, n']) : res
    where 
      n' = succ' n (concatMap snd res)
      res = whileToCFG' (n + 1) (n + 1) b 

whileToCFG' :: Int -> Int -> [Statement] -> CFG 
-- first int is the position of while
-- second int is incremented to create new indexes
-- checks for singleton to ensure succ points to start of while loop
whileToCFG' start n [Assign id e] 
  = [((id, usedVars e),[start])]
whileToCFG' start n [While p b] 
  =(("_", usedVars p), [n, n']) : res
    where 
      n' = succ' n (concatMap snd res)
      res = whileToCFG' (n + 1) (n + 1) b
whileToCFG' start n ((Assign id  e):ss) 
  = ((id, usedVars e),[n]) : whileToCFG' start (n + 1) ss 
