{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe (fromJust)
data Colour = Red | Green | Blue
  deriving (Show,Bounded,Enum)

mB :: Colour 
mB = minBound

afterGreen :: Colour 
afterGreen = succ Red

blueIntRep :: Int 
blueIntRep = fromEnum Blue

zeroEnumRep :: Colour 
zeroEnumRep = toEnum 0

allColours :: [Colour]
allColours = enumFromTo minBound maxBound

data Time = Full Hours Minutes | Half Hours Minutes AmPm

type Hours   = Int 
type Minutes = Int 
data AmPm = Am | Pm 
  deriving (Show,Enum,Eq)

to24 :: Time -> Time 
to24 (Half h m amPm)
  | amPm == Am = Full h m
  | otherwise  = Full (h+12) m
to24 t = t

equalTime :: Time -> Time -> Bool 
equalTime t1 t2 = to24 t1 == to24 t2

instance Eq Time where 
    t1 == t2 = equalTime t1 t2

instance Show Time where 
    show (Full h m)      = show h ++ show m ++ "hrs"
    show (Half h m amPm) = show h ++ ":" ++ show m ++ show amPm

type VarName = String
data Fun = Add | Sub | Mul
  deriving (Eq, Show)

data Exp = Val Int | Id VarName | App Fun Exp Exp
  deriving (Eq, Show)

type Assignment = (VarName, Exp)

type Program = [Statement]

data Statement = A Assignment | Loop Int Program

type Environment a = [(String, a)]

infixl 1 <--
(<--) :: VarName -> Exp -> Statement
(<--)= (A .) . (,)

loop :: Int -> Program -> Statement
loop = Loop

class Vars a where
    x, y, z :: a

p1 :: Program
p1 = [x <-- 8,
      y <-- 0,
      z <-- 11,
      loop 2 [ x <-- x + z - y,
               z <-- x - y + z,
               y <-- 2 * y - x]]
               
instance Vars String where 
    x = "x"
    y = "y"
    z = "z"

instance Vars Exp where 
    x = Id "x"
    y = Id "y"
    z = Id "y"

lift :: Fun -> Exp -> Exp -> Exp 
lift = App

instance Num Exp where 
  (+) = lift Add
  (-) = lift Sub 
  (*) = lift Mul 
  fromInteger = Val . fromInteger
  abs = abs
  signum = signum 

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = (fromJust .) . lookup

update :: VarName -> Int -> Environment Int -> Environment Int
update v n env = (v,n) : [p | p@(v',_) <- env, v /= v']

eval :: Exp -> Environment Int -> Int
eval (Val n)  env     = n 
eval (Id  id) env     = lookUp id env
eval (App f e e') env = apply f (eval e env) (eval e' env) 

apply :: Num a => Fun -> a -> a -> a
apply op = lookUp op [(Add, (+)),(Sub, (-)), (Mul, (*))]

evalS :: Statement -> Environment Int -> Environment Int
evalS (A (v, e)) env = update v (eval e env) env
evalS (Loop n p) env = foldr run' env (replicate n p)

run :: Program -> Environment Int
run p = run' p []
  
run' :: [Statement] -> Environment Int -> Environment Int
run' p env =  foldr evalS env (reverse p)
