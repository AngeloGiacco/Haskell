
module SOL where

import Data.List
import Data.Maybe ()

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x ((a,b):xs)
  | x == a    = b 
  | otherwise = lookUp x xs
lookUp _ [] = error "not found in list"

-- 3 marks
vars :: Formula -> [Id]
vars = sort . nub . vars' []
  where 
    vars' :: [Id] -> Formula -> [Id]
    vars' acc (And a b) = vars' acc a ++ vars' acc b 
    vars' acc (Or a b)  = vars' acc a ++ vars' acc b 
    vars' acc (Not x)   = vars' acc x
    vars' acc (Var id)  = id:acc 

-- 1 mark
idMap :: Formula -> IdMap
idMap x = zip (vars x) [1..] 

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not x))   = toNNF x
toNNF (Not (Or a b))  = And (toNNF $ Not a) (toNNF $ Not b)
toNNF (Not (And a b)) = Or  (toNNF $ Not a) (toNNF $ Not b)
toNNF (Var x)         = Var x
toNNF (Not x)         = Not x
toNNF (And a b)       = And (toNNF a) (toNNF b)
toNNF (Or a b)        = Or (toNNF a) (toNNF b)

-- 3 marks
toCNF :: Formula -> CNF
toCNF = nnfToCNF . toNNF 
  where 
    nnfToCNF :: NNF -> CNF 
    nnfToCNF (Or x y)  = distribute (nnfToCNF x) (nnfToCNF y)
    nnfToCNF (And x y) = And (nnfToCNF x) (nnfToCNF y)
    nnfToCNF (Not x)   = Not x
    nnfToCNF (Var x)   = Var x

-- 4 marks
flatten :: CNF -> CNFRep
flatten x = flatten' x (idMap x) 
  where 
    flatten' :: CNF -> IdMap -> CNFRep 
    flatten' (And x y) idM = represent x idM : flatten' y idM
    flatten' y         idM = [represent y idM]

represent :: Formula -> IdMap ->  [Int]
represent (Var x) idM  = [lookUp x idM] 
represent (Not x) idM  = map negate $ represent x idM 
represent (Or x y) idM = represent x idM ++ represent y idM

--------------------------------------------------------------------------
-- Part III

-- 5 marks
-- first output  = TheCNFRepthat results from propagating all unit clauses in the originalCNFRep
-- second output = The list of all unit clauses propagated as part of this process
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnfR = propUnits' (cnfR,[])
  where
    propUnits' :: (CNFRep, [Int]) -> (CNFRep, [Int])
    propUnits' (rep,lst)
      | null singleton = (rep,lst)
      | otherwise      = propUnits' $ simplify (rep,lst) (head singleton)
        where 
          singleton = filter isSingleton rep
          simplify :: (CNFRep, [Int]) -> [Int] -> (CNFRep, [Int])
          simplify (rep,lst) [x] = ((clauseDeletion x . literalDeletion x) rep, x:lst)

clauseDeletion :: Int -> CNFRep -> CNFRep 
clauseDeletion n = filter (n `notElem`)

literalDeletion :: Int -> CNFRep -> CNFRep 
literalDeletion n = map (filter (/= negate n))

isSingleton :: [Int] -> Bool 
isSingleton x = [head x] == x

-- 4 marks
dp :: CNFRep -> [[Int]]
dp = dp' .propUnits

dp' :: (CNFRep, [Int]) -> [[Int]]
dp' ([],[])                  = []
dp' ([],acc)                 = [acc] 
dp' ([]:_,_)                 = []
dp' (rep@((x:_):_),acc)
  | any null rep = [] 
  | otherwise    =  map (acc ++) (dp' (propUnits ([x]:rep))) 
                 ++ map (acc ++) (dp' (propUnits([negate x]:rep)))
  

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f = map (intRepToIdBoolMap (idMap f)) res    
  where 
    res = allSatIntRep f
    
allSatIntRep :: Formula -> [[Int]] 
allSatIntRep = dp . flatten . toCNF
    
intRepToIdBoolMap :: IdMap -> [Int] -> [(Id, Bool)]
intRepToIdBoolMap idM = map (rep idM) 
    
rep :: IdMap -> Int -> (Id,Bool)
rep idM n 
  | n < 0     = (reverseLookUp (negate n) idM,False)
  | otherwise = (reverseLookUp n idM,True)

reverseLookUp :: Eq a => a -> [(b, a)] -> b
-- Pre: The item being looked up has a unique binding in the list
reverseLookUp x ((f,s):xs)
  | x == s    = f
  | otherwise = reverseLookUp x xs
reverseLookUp _ [] = error "not found in list"


