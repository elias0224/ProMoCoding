--first task
--let lawful_to_chaotic = ["Lawful", "Neutral", "Chaotic"] 
--let good_to_evil = ["Good", "Neutral", "Evil"]
--[lc++" "++ge | lc <- lawful_to_chaotic, ge <- good_to_evil, lc /= gc]
--seems to only works in the terminal and waiting for solutions from university

--second task
alleGleich :: Integer -> Integer -> Integer -> Bool
alleGleich a b c = a==b && a==c

ungerade :: Integer -> Bool
ungerade a = a `mod` 2 /= 0

gerade :: Integer -> Bool
gerade b = b `mod` 2 == 0

--third task
--a)
geradeliste :: [Integer] -> [Integer]
geradeliste n = [x | x <- n, x `mod` 2 == 0 ]

--b)
geradeZahlen2 :: [Integer] -> [Integer]
geradeZahlen2 b = [if gerade a then a else a*2 | a <- b]

--c)
div8Rest4 :: Integer -> Integer -> [Integer]
div8Rest4 c d = [e | e <- [c..d], e `mod` 8 == 4]

--d)
leng :: String -> Integer
leng c = sum[1 | _ <- c]

--e)
vokale :: [Char] -> [Char]
vokale k = [p | p <- k,p `elem` "aeiouAEIOU" ]

--f)
faktoren :: Integer -> [Integer]
faktoren n = [e | e <- [1..n], n `mod` e == 0]

--g)
ggT :: Integer -> Integer -> Integer 
ggT n e = maximum[b | b <- faktoren n  , b `elem` faktoren e]

--h)
pytri :: Integer -> [(Integer, Integer, Integer)]
pytri h = [(a,b,c) | a <- [1..h], b <- [1..h], c <- [1..h], a*a == b*b + c*c ]
