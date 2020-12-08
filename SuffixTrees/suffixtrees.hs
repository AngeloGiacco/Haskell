module Practice where

import Data.List

isPrefix :: String -> String -> Bool
isPrefix pre word
  | length pre > length word = False 
  | otherwise                = and $ zipWith (==) pre word

removePrefix :: String -> String -> String
--Pre: first argument is a prefix of the second
removePrefix pre = drop (length pre) 


suffixes :: [a] -> [[a]]
suffixes []       = []
suffixes x@(_:xs) = x : suffixes xs

isSubstring :: String -> String -> Bool
isSubstring _ [] = False
isSubstring sub word@(_:ws) = isPrefix sub word || isSubstring sub ws

findSubstrings :: String -> String -> [Int]
findSubstrings target word = [x | 
                              x <- [0..length word - length target], 
                              isPrefix target $ drop x word]

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
  deriving (Eq,Ord,Show)

t0 :: SuffixTree
t0 = Node [("",Leaf 4),("",Leaf 1)]

t1 :: SuffixTree
t1= Node [("banana", Leaf 0),("a", Node [("na", Node [("na", Leaf 1),("", Leaf 3)]),
          ("", Leaf 5)]),("na", Node [("na", Leaf 2),("", Leaf 4)])]

t2 :: SuffixTree
t2= Node [("mississippi", Leaf 0),("i", Node [("ssi", Node [("ssippi", Leaf 1),("ppi", Leaf 4)]),
          ("ppi", Leaf 7),("", Leaf 10)]),("s", Node [("si", Node [("ssippi", Leaf 2),("ppi", Leaf 5)]),
          ("i", Node [("ssippi", Leaf 3),("ppi", Leaf 6)])]),("p", Node [("pi", Leaf 8),("i", Leaf 9)])]

getIndices :: SuffixTree -> [Int]
getIndices = getIndices' []
  where 
    getIndices' :: [Int] -> SuffixTree -> [Int]
    getIndices' acc (Leaf x)  = x:acc
    getIndices' acc (Node xs) =  acc ++ concatMap (\(z,x) -> getIndices' [] x) xs

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' = findSubstrings'' []
  where
    findSubstrings'' :: [Int] -> String -> SuffixTree -> [Int]
    findSubstrings'' acc s (Node ((label,i):xs))
      | shared == s     = acc ++ getIndices i 
      | shared == label = findSubstrings'' acc s' i 
      | otherwise       = findSubstrings'' acc s (Node xs)
        where 
          (shared,s',_) = partition' s label
    findSubstrings'' acc "" (Leaf x)   = x:acc 
    findSubstrings'' acc _ (Leaf x)    = acc

partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' a b = (x,a\\x,b\\x)
  where x = findShared a b 

findShared ::  Eq a => [a] -> [a] -> [a] 
findShared (a:as) (b:bs) 
  | a == b    = a : findShared as bs
  | otherwise = []
findShared _ _ = []

buildTree :: String -> SuffixTree
buildTree s = foldl (flip Practice.insert) (Node []) (zip (suffixes s) [0..])

insert :: (String,Int) -> SuffixTree -> SuffixTree 
insert x (Node xs) = Node (insert' x xs)
  where 
    insert' :: (String,Int) -> [(String, SuffixTree)] -> [(String, SuffixTree)]
    insert' (s,n) (x@(a,t):xs) 
      | null shared = x : insert' (s,n) xs 
      | shared == a = (a,Practice.insert (first',n) t) : xs
      | otherwise   = (shared, Node [(first', Leaf n), (second',t)]) : xs
        where 
          (shared, first', second') = partition' s a
    insert' (s,n) _        = [(s, Leaf n)]

longestRepSubstr :: SuffixTree -> String 
longestRepSubstr (Node xs) = foldl longest "" (map longestRepSub xs) 
    
longestRepSub :: (String, SuffixTree) -> String
longestRepSub (_,Leaf _) = ""
longestRepSub (s, t@(Node xs)) 
  | leaves xs < 2 = ""
  | otherwise     = s ++ longestRepSubstr t 

leaves :: [(String,SuffixTree)] -> Int
leaves = foldr ((+) . leaves') 0
  where 
    leaves' :: (String,SuffixTree) -> Int
    leaves' (_,Leaf _)  = 1 
    leaves' (_,Node ts) = leaves ts

longest :: Eq a => [a] -> [a] -> [a]
longest a b = if a == zipWith const a b then b else a