import Data.Map ( Map, fromList, toList ) 
import Data.List ( map )
import Data.Bifunctor ( Bifunctor(second) )

class MyFunctor f where 
    myFmap :: (a -> b) -> f a -> f b

instance (Ord k) => MyFunctor (Data.Map.Map k) where
    myFmap f = Data.Map.fromList . Data.List.map (Data.Bifunctor.second f) . Data.Map.toList