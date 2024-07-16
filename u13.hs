{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.List



--A13-1)


-- instance Monoid Ordering where 
--     mempty       = EQ
--     mappend LT   = LT
--     mappend GT   = GT
--     mappend EQ x = x

data Team = Team {name :: String, nW :: Int, nD :: Int, nL :: Int, nGF :: Int, nGA :: Int} deriving (Eq, Show)

instance Ord Team where  
    compare :: Team -> Team -> Ordering
    compare x xs = (compare (nD x +3*nW x) (nD xs +3*nW xs)) <> (compare (nGF x - nGA x) (nGF xs - nGA xs)) <> (compare (name xs) ( name x))

--A13-2)

succ' ::Integer -> Integer 
succ' x = x + 1

pred' :: Integer -> Integer
pred' x = x - 1

opp' :: Integer -> Integer
opp' x = -x

 --a)
plus' :: Integer -> Integer -> Integer
plus' x 0 = x
plus' x y | y > 0           = plus' (succ' x) (pred' y)
          |otherwise        = minus' x (opp' y)

minus' :: Integer -> Integer -> Integer 
minus' xs 0 = xs
minus' xs ys | ys > 0   = minus' (pred' xs) (pred' ys)
             |otherwise = plus' xs (opp' ys)

mult' :: Integer -> Integer -> Integer
mult' = undefined

fact :: Integer -> Integer 
fact = undefined


--A13-3)
--a) g(2) = 2 * g(0)        g(0) terminiert nicht
--b) außer für [] terminiert es nicht, da ss immer mit der gleichen Liste arbeitet
--c) m(x:xs) = length (x:xs)> length (xs) = m(xs)
--d) A = N  
        -- fib(0)                             = 1 elem A
        -- fib(1)                             = 1 elem A
        -- fib(2) = fib(1)   + fib(0) = 1 + 1 = 2 elem A 
        -- fib(3) = fib(2)   + fib(1) = 2 + 1 = 3 elem A
        -- fib(x) = fib(x-1) * fib(x-2)
--e) 
        --abgeschlossen: basisfall tritt ein 

--A13-4)


class Default a where
    def :: a

instance Default Integer where
    def :: Integer
    def = 0


instance Default (Maybe a) where
    def :: Maybe a
    def = Nothing

-- instance (Num a) => Default a where
--       def = 0

instance Default () where
    def :: ()
    def = ()

instance (Default a, Default b) => Default (a,b) where
    def :: (Default a, Default b) => (a, b)
    def = (def, def)

instance Monoid a => Default a where
    def = mempty

instance Default b => Default (a -> b) where
    def :: Default b => a -> b
    def = \_ -> def
 

--A13-5)

functorReplace :: Functor f => a -> f b -> f a 
functorReplace x = fmap (const x)