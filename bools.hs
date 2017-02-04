myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if (x == True) 
    then True else (myOr xs)

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' xs = foldr (||) False xs
-- myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if (f x == True) 
    then True else (myAny f xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f [] = False
myAny' f xs = foldr (\x -> \y -> f x || y) False xs

newtype Any a = Any { getAny :: Bool } deriving Show
newtype All a = All { getAll :: Bool } deriving Show

-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a


instance Monoid (Any a) where
    mempty = Any False
    mappend (Any a) (Any b) = Any (a || b)

instance Monoid (All a) where
    mempty = All True
    mappend (All a) (All b) = All (a && b)

data Days = Sunday | Monday deriving Show

data Either a b = 
    Left a 
  | Right b deriving Show