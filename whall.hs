

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
rest x = (reverse(take ((length x )-1)(reverse x)))

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
leng x = sum[1 | _ <- x]

vokale :: [Char] -> [Char]
vokale x = [y | y <- x, y `elem` vokale1]

faktoren :: Integer -> [Integer]
faktoren x = [y | y <- [1..x], x `mod` y == 0]

ggT :: Integer -> Integer -> Integer
ggT x y = maximum[ z | z <- faktoren x , z `elem` faktoren y]

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
piApprox x = sqrt(6*sum([  1 /(fromIntegral(y*y))| y <- [1 .. x]]))

piApprox' :: Int -> Double
piApprox' x = sqrt(6*(piApprox'' x 0))

piApprox'' :: Int -> Int -> Double 
piApprox'' x n | x > n     = (1 / fromIntegral(x*x) + piApprox'' (x-1) 0) 
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