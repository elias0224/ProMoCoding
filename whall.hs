{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Trace.Hpc.Mix (CondBox(QualBinBox))
{-# HLINT ignore "Use odd" #-}



--Blatt1

double :: Integer -> Integer
double x = x*2

vierfach :: Integer -> Integer
vierfach x = x*4

kopf :: [a] -> a
kopf x = x!!0

ende :: [a] -> a
ende x = kopf (reverse x)

rest :: [a] -> [a]
rest x = (reverse (take ((length x )-1) (reverse x)))

start :: [a] -> [a]
start x = reverse (rest (reverse x))

--Blatt2

a = ["lawful", "Neutral", "Chaotic"]
b = ["Good", "Neutral", "Evil"]

vokale1 = "aeiou"


list :: Eq b => [b] -> [b] -> [(b, b)]
list x y = [(a, b)| a <- x, b <- y, a /= b]

alleGleich :: Integer -> Integer -> Integer -> Bool
alleGleich x y z = x == y && y ==z

ungerade :: Integer -> Bool
ungerade x = x `mod` 2 /= 0

gerade :: Integer -> Bool
gerade x  = x `mod` 2 == 0

listgerade :: [Integer] -> [Integer]
listgerade x = [y | y <- x, gerade y ]

listgeradedoppel :: [Integer] -> [Integer]
listgeradedoppel x = [if ungerade y then y*2 else y| y <- x]

div8rest4' :: Integer -> Integer -> [Integer]
div8rest4' x y = [z | z <- [x..y], z `mod` 8 == 4]

leng :: [Char] -> Integer
leng x = sum [1 | _ <- x]

vokale :: [Char] -> [Char]
vokale x = [y | y <- x, y `elem` vokale1]

faktoren :: Integer -> [Integer]
faktoren x = [y | y <- [1..x], x `mod` y == 0]

ggT :: Integer -> Integer -> Integer
ggT x y = maximum [ z | z <- faktoren x , z `elem` faktoren y]

pytri :: Integer -> [(Integer, Integer, Integer)]
pytri x = [(a, b, c)| a <- [1..x],b <- [1..x],c <- [1..x], (a*a)==(b*b)+(c*c)]

--Blatt3

note :: Double -> (Integer, String)
note x | x > 87.5 = (1, "sehr gut")
       | x > 75 = (2, "gut")
       | x > 62.5 = (3, "befriedigend")
       | x > 50 = (4, "ausreichend")
       |otherwise = (5, "nicht ausreichend")


piApprox :: Int -> Double
piApprox x = sqrt (6*sum ([  1 /(fromIntegral (y*y))| y <- [1 .. x]]))

piApprox' :: Int -> Double
piApprox' x = sqrt (6*(piApprox'' x 0))

piApprox'' :: Int -> Int -> Double
piApprox'' x n | x > n     = (1 / fromIntegral (x*x) + piApprox'' (x-1) 0)
               |otherwise  = 0

length' ::[a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' x = length''' x 0
    where
        length''' :: [a] -> Int -> Int
        length''' [] acc = acc
        length''' (x:xs) acc = length''' xs (acc + 1 )

append' :: [a] -> [a] -> [a]
append' _ [] = []
append' [] _ = []
append' (x:xs) (y:ys) = x:y: append' xs ys

append'' :: [a] -> [a] -> [a]
append'' x y = reverse (append''' x y [])
    where
        append''' :: [a] -> [a] -> [a] -> [a]
        append''' _ [] acc = acc
        append''' [] z acc = acc ++ z
        append''' (x:xs) (y:ys) acc = append''' xs ys (x:acc)

contains' :: Eq a => [a] -> a -> Bool
contains' (x:xs) n | n == x = True
                   |otherwise = False || contains' xs n

contains'' :: Eq a => [a] -> a -> Bool
contains'' x n = contains''' x n False
    where
        contains''' :: Eq a => [a] -> a -> Bool -> Bool
        contains''' (x:xs) n acc | x == n = False
                                |otherwise = acc || contains''' xs n acc

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++[x]

reverse'' :: [a] ->  [a]
reverse'' x = reverseacc x []
    where
        reverseacc :: [a] -> [a] -> [a]
        reverseacc [] acc = acc
        reverseacc (x:xs) acc = reverseacc xs (x:acc)

--Blatt4

palindrome' :: String -> Bool
palindrome' []                    = True
palindrome' [_]                   = True
palindrome' x | head x  == last x = palindrome' (init (tail x))
             |otherwise          = False

-- sieve' :: Int -> [Int]
-- sieve' x = 2: [y| y <- [2..x], y `mod` 2 /= 0 && y `mod` 3 /= 0]



-- Blatt 5

data Player =  Player {name :: String, team :: String, number :: Int, foot :: Foot, position :: Position } deriving (Show, Eq)
data Foot= LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)

a1 = Player "Jonah" "Heim" 99 LeftF Midfielder
a2 = Player "Joni" "Heim" 90 RightF Goalkeeper
a3 = Player "Joni" "Heim" 90 RightF Goalkeeper

sameTeam :: [Player] -> Bool
sameTeam [] = True
sameTeam (x:xs) = all (\player -> team player == team x) xs

uniqueNumbers :: [Player] -> Bool
uniqueNumbers [] = True
uniqueNumbers (x:xs) = (notElem x xs )&& uniqueNumbers xs

coverPosition :: [Player] -> Bool
coverPosition p = all(\positions -> positions `elem` map position p) [Goalkeeper, Defender, Midfielder, Forward]

--Blatt 6

data Astronaut = Astronaut {benutzername :: String, rolle :: Rolle, zustand :: Zustand, aufgaben :: [String], color :: Color} 

data Rolle = Crewmate | Imposter deriving (Enum,Show,Eq)
data Zustand = Tot | Lebendig deriving (Enum, Show,Eq)
data Color = Rot | Blau | Lila | Gelb | Rosa | Orange deriving (Enum,Show,Eq)

instance Show Astronaut where
    show :: Astronaut -> String
    show (Astronaut x _ _ _ y) = x ++ ":"  ++ show y

instance Eq Astronaut where
    (==) :: Astronaut -> Astronaut -> Bool
    (Astronaut a _ _ _ b) == (Astronaut a'  _ _ _ b') = a == a' && b == b'

istCrewmate :: Astronaut -> Bool
istCrewmate x = rolle x == Crewmate

istImposter :: Astronaut -> Bool
istImposter x = rolle x == Imposter

istLebendig :: Astronaut -> Bool
istLebendig x = zustand x == Lebendig

istTot :: Astronaut -> Bool 
istTot x = zustand x == Tot 

data ML a = E | L a (ML a) deriving (Show, Eq)

l1 = L 1 (L 2 (L 3 (L 4 E)))
l2 = L 2 (L 3 (L 4 (L 5 E)))

l3 = L 2 (L 3 (L 4 E))


myHead :: ML a -> a 
myHead E       = error "empty list" 
myHead (L x _) = x

myAppend :: ML a -> ML a -> ML a
myAppend E xs = xs
myAppend (L x y) xs = (L x (myAppend y xs))

myAdd :: (Eq a, Num a) => ML a -> ML a -> ML a
myAdd E _ = E
myAdd _ E = E 
myAdd (L x y) (L z c) = (L (x+z) (myAdd y c))

toString :: Show a => ML a -> String
toString E = ""
toString (L x E) = show x ++ " "
toString (L x y) = show x ++ ", " ++ toString y

myLess :: Ord a => ML a -> ML a -> Bool
myLess _ E = False
myLess E _ = False
myLess (L x E) (L y E) = x < y
myLess (L x y) (L c v) = (x < c )&& myLess y v

any' :: (a -> Bool) -> [a] -> Bool
any' f (x:xs) = f x && any' f xs

map' :: (a -> b) -> [a] -> [b]
map' f x = foldr (\x acc -> f x : acc) [] x

reversee :: [a] -> [a]
reversee  = foldl (\acc x -> x:acc)  []

zipwithh :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwithh f [] x = []
zipwithh f y [] = []
zipwithh f (x:xs) (y:ys) = [(f x y)] ++ zipwithh f xs ys

unzipwithh :: (t -> (a,b)) -> [t] -> ([a],[b])
unzipwithh f [] = ([],[])
unzipwithh f (x:xs) = let (a, b) = f x
                          (as, bs)  = unzipwithh f xs
                      in (a:as, b:bs)

lengthh :: Num a => [a] -> a
lengthh = foldl (\acc _ -> acc +1 ) 0

filterr :: (a -> Bool) -> [a] -> [a]
filterr f = foldr (\x acc -> if f x then x: acc else acc) []

alll :: (a -> Bool) -> [a] -> Bool
alll f = foldr (\x acc -> if f x then True && acc else False) True

elemm :: Eq a => a -> [a] -> Bool
elemm y z = foldr (\x acc -> (x == y ) || acc) False z

takewhilee :: (a -> Bool) -> [a] -> [a]
takewhilee f x = foldr (\x acc -> if f x then x: acc else acc) [] x

ungeradequadrate :: Integer 
ungeradequadrate = sum .takewhilee (<10000) $ map (^2) $ filter odd [1..]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n:chain (n `div` 2)
        | odd n = n:chain (n * 3 + 1)

chainlength :: Int
chainlength = length . filter (\x -> length x > 15) . map chain $ [1..100]

data ComplexNumber = C {re :: Double, im :: Double}

instance Show ComplexNumber  where
    show :: ComplexNumber -> String
    show x = show (re x) ++ " + " ++  show (im x) ++ "i"

instance Semigroup ComplexNumber where 
    (<>) :: ComplexNumber -> ComplexNumber -> ComplexNumber
    (<>) x xs =C ((re x) * (re xs) - (im x) * (im xs) ) ((re x) * (im xs) - (re xs) *(im x))

data RGB = RGB {rot::Int, grün::Int, blau::Int}

instance Semigroup RGB where
    (<>) :: RGB -> RGB -> RGB 
    (<>) x y = RGB (addcolor (rot x) (rot y)) ( addcolor (grün x) (grün y)) (addcolor (blau x) (blau y))
        where 
            addcolor :: Int -> Int -> Int
            addcolor x y | (x+y) > 255 = 255
                         | otherwise = x+y

