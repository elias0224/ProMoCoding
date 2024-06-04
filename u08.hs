--A8-1

--a)
data C = C { re::Double , im :: Double}

instance Semigroup C where
    (<>) :: C -> C -> C
    (<>) x xs =  C (re x * re xs- im x * im xs) (re x * im xs - im x * re xs)

instance Show C  where
    show :: C  -> String
    show x = show (re x) ++ " + " ++ show (im x) ++ "i"


--b)

data RGB = RGB { rot::Int, grün::Int, blau::Int}

instance Semigroup RGB where
    (<>) :: RGB -> RGB -> RGB
    (<>) x y =  RGB (addc (rot x) (rot y) ) (addc (grün x) (grün y)) (addc (blau x) (blau y))
            where addc :: Int -> Int -> Int
                  addc x y  | (x+y) > 255 = 255
                            | otherwise = x+y


instance Monoid RGB where
    mempty :: RGB 
    mempty = RGB 0 0 0 

--A8-2

--a)

op :: Int -> Int -> Int
op x xs | x /= 0 && x /= 1 && xs /= 0 && xs /= 1 = error ""
        | x /= xs                                = 0 
        | x == xs                                = 1

--b)


--c)

instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) x xs | x == 1 || xs == 1  = x + xs -1
              | x == xs            = 1
              | x /= xs            = 0

instance Monoid Int where
    mempty :: Int
    mempty = 1





--A8-3

--a)
data Triple a = Triple a a a deriving (Eq)
--Beispiel
a1 = Triple 1 2 3
--Triple {one :: a, two :: a, third :: a } deriving Eq

instance (Show a) => Show (Triple a) where
    show :: (Show a) => Triple a -> String
    show (Triple x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

--b)
tfst :: Triple a -> a
tfst (Triple x _ _) = x

tsnd :: Triple a -> a
tsnd (Triple _ x _) = x

tthird :: Triple a -> a
tthird (Triple _ _ x) = x

--c)

triplefromList :: [a] -> Triple a
triplefromList []         = error "kein Punkt"
triplefromList [_]        = error "kein Punkt"
triplefromList [_,_]      = error "kein Punkt"
triplefromList (x:y:z:xs) = Triple x y z


tripletoList :: Triple a -> [a]
tripletoList (Triple x y z) = [x, y, z]

--d)

x :: Num a => Triple a -> Triple a -> Triple a
x (Triple x y z) (Triple x' y' z') = Triple (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')

--e)

instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)

scaMult :: Num a => a -> Triple a -> Triple a
scaMult s = fmap (*s)

