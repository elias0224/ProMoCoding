{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use even" #-}
import Trace.Hpc.Mix (CondBox(QualBinBox), BoxLabel)
import GHC.Exception (getCallStack)
{-# HLINT ignore "Use odd" #-}
import System.IO
import Data.Char
import Control.Monad
import Data.List


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
coverPosition p = all (\positions -> positions `elem` map position p) [Goalkeeper, Defender, Midfielder, Forward]

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

instance Monoid RGB where
    mempty :: RGB
    mempty = RGB 0 0 0

op :: Int -> Int -> Int
op x y | x == 0 && y == 0 = 1
       | x == 0 && y == 1 = 0
       | x == 1 && y == 0 = 0
       |otherwise         = 1



instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) x y | x == 0 && y == 0 = 1
             | x == 0 && y == 1 = 0
             | x == 1 && y == 0 = 0
             |otherwise         = 1


instance Monoid Int where
    mempty :: Int
    mempty = 0

t1 = Triple 1 2 3

data Triple a =  Triple a a a deriving (Eq)

instance (Show a) => Show (Triple a) where
    show :: Show a => Triple a -> String
    show (Triple x y z) = "Triple: " ++ show x ++ ", " ++ show y ++ ", " ++ show z

fstt :: Triple a -> a
fstt (Triple x y z) = x

sndd :: Triple a -> a
sndd (Triple x y z) = x

trdd :: Triple a -> a
trdd (Triple x y z) = x

triplefromList :: [a] -> Triple a
triplefromList (x:y:z:xs) = Triple x y z

tripleToList :: Triple a -> [a]
tripleToList (Triple x y z) = [x, y, z]

x :: Num a => Triple a -> Triple a -> Triple a
x (Triple x y z) (Triple f g h) = (Triple (y*h-z*g) (z*f-x*h) (x*g-y*f))

instance Functor Triple  where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)


scaMult :: Num a => a -> Triple a -> Triple a
scaMult x  = fmap (*x)



--Blatt 9

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving Show

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

b1 = (Node 6 (Node 1 Empty (Node 3 Empty (Node 5 Empty Empty )) ) (Node 8 (Node 9 Empty Empty) (Node 12 Empty Empty)))

istBSt :: Ord a => BinTree a -> Bool
istBSt Empty = False
istBSt (Node x Empty Empty) = True
istBSt (Node x y z) = cTree ( <= x) y && cTree (> x) z where
        cTree :: Ord a => (a -> Bool) -> BinTree a -> Bool
        cTree f (Node u i l) = (f u) && cTree f i && cTree f l

depth :: (Num a, Ord a) => BinTree t -> a
depth Empty = 0
depth (Node x y z) = 1 + maximum (depth y, depth z)

insertt :: Ord a => a -> BinTree a -> BinTree a
insertt x Empty = (Node x Empty Empty)
insertt x (Node z y u) | x > z = Node z y (insertt x u)
                      |otherwise = Node z (insertt x y) u


elem':: Ord a => a -> BinTree a -> Bool
elem' x Empty = False
elem' x (Node z y u) | x == z = True
                     | x > z = False || elem' x u
                     |otherwise = False || elem' x y

leavess :: Num a => BinTree b -> a
leavess Empty = 0
leavess (Node x Empty Empty) = 1
leavess (Node x y z) = leavess y + leavess z

data Term  a = Const Int | PLUS (Term a )(Term a)| TIMES (Term a) (Term a) | MINUS (Term a) (Term a)| DIVISION (Term a) (Term a)| Neg (Term a) deriving Show

evall :: Integral a => Term a -> Int
evall (Const x) = x
evall (PLUS x y) = (evall x) + (evall y)
evall (TIMES x y) = (evall x) * (evall y)
evall (MINUS x y) = (evall x) - (evall y)
evall (DIVISION x y) = (evall x) `div` (evall y)
evall (Neg x) =  -(evall x)

simplify :: Term a -> Term a
simplify (Neg(Neg x)) = x
simplify (PLUS x (Neg y)) = (MINUS x y)
simplify (MINUS x (Neg y)) = (PLUS x y)


--Blatt10


-- main :: IO()
-- main = do  
--     putStr "Waehlen Sie einen Gesangstil: "
--     stil <- getLine 
--     putStr "Wie ist Ihr Name? "
--     name <- getLine
--     putStr "Hier ist Ihr Lied: "
--     file <- readFile "u10/one-hit-wonder.txt" 
--     let changedname = changeName name file
--     if (stil == "laut") 
--         then putStrLn (map toUpper file)
--     else if (stil == "leise") 
--         then putStrLn (map toLower file) 
--         else putStrLn file

-- changeName :: String -> String -> String
-- changeName name file = unlines (map ((unwords . wordsstoName name "M" ). words)( lines file))

-- wordsstoName :: String -> String -> [String] -> [String]
-- wordsstoName _ _ [] = []
-- wordsstoName name word (x:xs) | x == word = name : wordsstoName name word xs
--                               |otherwise= x: wordsstoName name word xs

-- main:: IO()
-- main = do 
--     putStr "Ein deutsches Wort bitte: "
--     dword <- getLine
--     if(null dword )
--         then do
--         putStr "Auf Wiedersehen!"
--         return ()
--     else do
--         putStr "und das bairrische Wort bitte : "
--         bword <- getLine
--         appendFile "WB.txt" (show (dword,bword))
--         putStrLn ("" ++ dword ++ " heisst auf Bairisch " ++ bword ++ "")
--         main

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    contentsWB <- readFile "WB.txt"
    let dict = map read (lines contentsWB)

    putStr "Geben sie ein hochdeutsches Wort ein "
    word <- getLine

    let translation = dictLookup dict word
    if  null translation
        then putStrLn word
        else putStr $ unlines translation
     

    -- text <- readFile "orginal.txt"
    -- let text2 = words text
    -- let translation1 = übersetzer dict text2
    -- if null translation1
    --     then do
    --         putStr text
    --     else do
    --         putStr (unlines translation1)




dictLookup :: [(String,String)] -> String -> [String]
dictLookup dict word = map snd $ filter ((==word). fst) dict

übersetzer :: [(String,String)] -> [String] -> [String]
übersetzer _ [] = []
übersetzer dict (x:xs) = dictLookup dict x ++ übersetzer dict xs




--Blatt 11

data List a = Nil | Cons a (List a) deriving (Show,Eq)

negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor = fmap (negate)


instance Functor List  where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil 
    fmap f (Cons x y) = (Cons (f x) (fmap f y))

instance Applicative List where
    pure :: a -> List a
    pure x = (Cons x (pure x ))
    (<*>) :: List(a -> b) -> List a -> List b
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons x y) (Cons a b) = Cons (x a)  (y <*> b)

zipwith'' :: (a -> b -> c) -> List a -> List b -> List c
zipwith'' f x xs = f <$> x <*> xs

type Money = Int

type Account = (Money, Money)

withdraw :: Money -> Account -> Maybe Account
withdraw x (debit, credit) |  debit + credit >= 0        = Just (debit, credit -x)
                           |otherwise                    = Nothing

deposit :: Money -> Account -> Maybe Account
deposit x (debit, credit) |  debit + credit >= 0    = Just (debit+x, credit)
                          |otherwise                = Nothing

type Balance = Money 

accstate :: Account -> Maybe Balance
accstate (debit, credit) | debit + credit >= 0 = Just (debit + credit)
                         |otherwise            = Nothing

data Box a = Emptyy String | Full a

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Emptyy x) = Emptyy x
    fmap f (Full a) = Full (f a)

instance Applicative Box where
    pure :: a -> Box a
    pure x = Full x
    (<*>) :: Box(a -> b) -> Box a -> Box b
    (<*>) (Emptyy x) _ = Emptyy x
    (<*>) _ (Emptyy x) = Emptyy x
    (Full x) <*> (Full b) = Full (x b)

instance Monad Box where
    return :: a -> Box a
    return = pure
    (>>=) :: Box a -> (a -> Box b) -> Box b
    (Full x) >>= k = k x
    Emptyy x >>= k = Emptyy x

    --Blatt 12

 
chainAction1 :: Monad m => a -> [(a -> m a)] -> m a
chainAction1 xs []      = return xs
chainAction1 xs (f:fs)  = do 
                x <- f xs
                chainAction1 xs fs


chainAction2 :: Monad m => a -> [(a -> m a)] -> m a
chainAction2 xs [] = return xs
chainAction2 xs (f:fs) = f xs >>= (\x -> chainAction2 x fs)

chainAction3 :: Monad m => a -> [(a -> m a)] -> m a
chainAction3 xs f= foldM (\xs f ->  f xs) xs f

tellOp :: (Show a, Show b) => (a -> b) -> a -> IO b
tellOp f x = let fx = f x in do
    putStrLn $ (show x) ++ " -> " ++ (show fx)
    return fx


test :: [Int -> IO Int]
test = map tellOp [ (*3), (+1), ( mod 7), (+5), (*2) ]
    

data Logger a = Logger a [String]

instance (Show a) => Show (Logger a) where
    show :: Show a => Logger a -> String
    show (Logger v logs) = show v ++ "\n" ++ unlines (reverse logs)

instance Functor Logger where
    fmap :: (a -> b) -> Logger a -> Logger b 
    fmap f (Logger x log) = Logger (f x) (log)

instance Applicative Logger where
    pure :: a -> Logger a
    pure x = Logger x []
    (<*>):: Logger (a -> b) -> Logger a -> Logger b 
    (<*>) (Logger x y) (Logger z t) = Logger (x z) (y ++ t)

instance Monad Logger where
    return :: a -> Logger a
    return = pure
    (>>=) :: Logger a -> (a -> Logger b) -> Logger b 
    (>>=) (Logger x xs) f = let (Logger y ys) = f x in (Logger y (xs ++ ys))

data Match = Match {homeTeam::String, awayTeam::String, homeScore::Int, awayScore::Int}

instance Show Match where
    show :: Match -> String
    show m = home ++ " - " ++ away 
        where home = homeTeam m ++ " " ++ show (homeScore m)
              away = show (awayScore m) ++ " " ++ awayTeam m



-- Given two teams (h and a), start the match
startMatch :: String -> String -> Logger Match
startMatch h a = Logger (Match h a 0 0) ["Start!"]


-- Finish the current match
endMatch :: Match -> Logger Match
endMatch m = Logger m ["Ende"]


-- Given name of player (p) and minute of game (t), add one goal to home team
scoreHome :: String -> Int -> Match -> Logger Match
scoreHome p t m = Logger ((Match)(homeTeam m) (awayTeam m) (homeScore m +1) (awayScore m)) ["Tor!! von : "++ p ++" in der " ++ show t ++" Minute."]


-- Given name of player (p) and minute of game (t), add one goal to away team
scoreAway :: String -> Int -> Match -> Logger Match
scoreAway p t m = Logger (Match (homeTeam m) (awayTeam m) (homeScore m) (awayScore m)) ["Tor!! von : "++ p ++ " in der " ++ show t ++ " Minute."]



type KnightPos = (Int, Int)


moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) | checkpos (x, y) = filter checkpos [(x+c, y+z*((c `mod`2)+1))|c <- [-2,-1,1,2], z <- [-1,1] ]
                  |otherwise = error "Ungültige Position"
        where
            checkpos :: KnightPos -> Bool
            checkpos (x, y) | x > 0 && x<9 && y> 0 && y<9 = True
                            |otherwise = False

moveKnightextra :: [KnightPos] -> [KnightPos]
moveKnightextra ((x, y):xs)| checkpos2 (x, y) = filter checkpos2 [(x+c, y+z*((c `mod`2)+1))|c <- [-2,-1,1,2], z <- [-1,1] ] ++ moveKnightextra xs
                  |otherwise = error "Ungültige Position"
        where
            checkpos2 :: KnightPos -> Bool
            checkpos2 (x, y) | x > 0 && x<9 && y> 0 && y<9 = True
                            |otherwise = False

in3Movess :: KnightPos -> [KnightPos] 
in3Movess (x, y) = in3Moves' (in3Moves' (moveKnight (x, y) ))
    where 
        in3Moves' :: [KnightPos] -> [KnightPos]
        in3Moves' []     = []
        in3Moves' (x:xs) = moveKnight x ++ in3Moves' xs

reachin3Moves :: KnightPos -> KnightPos -> Bool
reachin3Moves x y = y `elem` (in3Movess x)

inxMoves :: Int -> KnightPos -> [KnightPos]
inxMoves x y | x>=2 = moveKnightextra (inxMoves (x-1) y)
             |otherwise = moveKnight y   

reachxMoves :: Int -> KnightPos -> KnightPos -> Bool
reachxMoves x y z = y `elem` inxMoves x z    

-- data  Ordering    =  LT | EQ | GT deriving
--                                   (Eq, Ord, Bounded, Enum, Read, Show)

data Team = Team {namee::String, nW:: Int, nD::Int, nL::Int,nGF:: Int, nGA:: Int} deriving (Eq,Show)

instance Ord Team where
    compare :: Team -> Team -> Ordering
    compare x xs = (compare (nD x + 3*nW x) (nD xs + 3*nW xs) )<> (compare (nGF x-nGA x)(nGF xs - nGA xs)) <> (compare (namee x)( namee xs))


