import Distribution.Simple.Command (OptDescr(BoolOpt))
--A 3.1
note :: Double -> (Integer, String)
note n
    |n >= 87.5 = (1, "sehr gut")
    |n >= 75.0 = (2, "gut")
    |n >= 62.5 = (3, "befriedigend")
    |n >= 50.0 = (4, "ausreichend")
    |otherwise = (5, "nicht ausreichend")

--A 3.2
piApprox :: Int -> Double
piApprox n = sqrt(6*sum[1 / fromIntegral(b*b)|b <- [1..n]])

piApprox' :: Int -> Double
piApprox' n = sqrt (piApproxRec n 1)

piApproxRec :: Int -> Int -> Double
piApproxRec n k
         | k > n     = 0
         | otherwise = 1 / (fromIntegral (k * k)) + piApproxRec n (k + 1)

--A 3.3

--a)
laenge :: [a] -> Int
laenge [] = 0
laenge (x:xs) = 1 + length xs 

--b)
append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' xs [] = xs
append' (x:xs) ys = x : append' xs ys

--b)endrekursiv
append'' :: [a] -> [a] -> [a]
append'' xs ys = append''acc xs ys []

append''acc :: [a] -> [a] -> [a] -> [a]
append''acc []  ys acc = acc ++ys
append''acc (x:xs) ys acc = append''acc xs ys (acc ++ [x])

--c)
contains' :: Eq a => [a] -> a -> Bool
contains' [] y = False
contains' (x:xs) y 
                | x == y    = True
                | otherwise = contains' xs y

--c)endrekursiv
contains'' :: Eq a => [a] -> a -> Bool
contains'' x y = contains''acc x y False
        where
            contains''acc :: Eq a => [a] -> a -> Bool -> Bool
            contains''acc [] _ acc = acc
            contains''acc (x:xs) y acc
                            |x == y  =True
                            |otherwise = contains''acc xs y acc

--d)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

--d)endrekursiv
reverse'' :: [a] -> [a]
reverse'' x = reverse''acc x []
    where 
        reverse''acc :: [a] -> [a] -> [a]
        reverse''acc [] acc = acc
        reverse''acc (x:xs) acc = reverse''acc xs (x:acc)

--e)
take' :: (Num a, Ord a) => a -> [a] -> [a]
take' _ [] = []
take' y (x:xs)
        | y <= 0  = []
        | otherwise = x : take' (y-1) xs

--e)endrekursiv
take'' :: (Num a, Ord a) => a -> [a] -> [a]
take'' y x = take''acc y x []

take''acc :: (Num a, Ord a) => a -> [a] -> [a] -> [a]
take''acc _ [] acc = acc
take''acc y (x:xs) acc
                | y <= 0 = acc
                |otherwise = take''acc (y-1) xs (acc ++[x])