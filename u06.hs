
a1 = Astronaut "Mario" Crewmate Lebendig ["Rettet Peach"] Rot


--a)
data Astronaut = Astronaut {benutzername :: String, rolle :: Rolle, zustand :: Zustand, aufgaben :: [String], farbe :: Farbe}

--b)
data Rolle     = Imposter | Crewmate deriving (Eq, Show)
data Zustand   = Lebendig | Tot deriving (Eq, Show)
data Farbe     = Rot | Blau | Lila | Gelb | Rosa | Orange deriving (Enum, Eq, Show)

--c)

instance Eq Astronaut where
    (==) :: Astronaut -> Astronaut -> Bool
    Astronaut x _ _ _  z == Astronaut x' _ _ _  z' = x == x' && z == z'

instance Show Astronaut where
    show :: Astronaut -> String
    show (Astronaut x _ _ _ y) = x ++ ":" ++ show y

--d)
istCrewmate :: Astronaut -> Bool
istCrewmate x = rolle x == Crewmate

istImposter :: Astronaut -> Bool
istImposter x = rolle x == Imposter

istLebendig :: Astronaut -> Bool
istLebendig x = zustand x == Lebendig

istTot :: Astronaut -> Bool
istTot x = zustand x == Tot


--A6-2
--a)

data ML a = E | L a (ML a) deriving (Show)

liste1 :: ML Integer
liste1 = L 1 (L 2 (L 3 (L 4 E)))

--b)
myHead :: ML a -> a
myHead  E      = error "empyt list"
myHead (L x _) = x

--c)
myAppend :: ML a -> ML a -> ML a
myAppend x E = x
myAppend E y = y
myAppend (L x y) z = L x (myAppend y z)

--d)
myAdd :: Num a => ML a -> ML a -> ML a
myAdd E x = E
myAdd y E = E
myAdd (L x y) (L s r) = L (x + s) (myAdd y r)

--e)
toString :: Show a => ML a -> [Char]
toString E = ""
toString (L x E) = show x
toString (L x y) = show x  ++ ","  ++ toString y

--f)
myLess :: Ord a => ML a -> ML a -> Bool
myLess E _ = False
myLess _ E = False
myLess (L x E) (L x' E)   = x < x'
myLess (L x y) (L x' y') = (x < x') && myLess y y'

l1 = L 1 (L 2 (L 3 E))
l2 = L 2 (L 3 (L 4 (L 4 E)))

--A6-3

--a) 

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (y:ys) = f y || any' f ys

--b)

map' :: (a -> b) -> [a] -> [b]
map' f  = foldr (\x acc -> f x : acc) []

--c)

reverse' :: [a] -> [a]
reverse'=  foldl (\acc x -> x : acc) []

--d)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith'  f xs ys

unzipWith' :: (t -> (a, b)) -> [t] -> ([a], [b])
unzipWith' _ [] = ([],[])
unzipWith' f (x:xs) = 
      let (a, b) = f x 
          (as, bs) = unzipWith' f xs 
      in  (a:as, b:bs)

