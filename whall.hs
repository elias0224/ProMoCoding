
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