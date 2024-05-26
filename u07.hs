{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

--A7-1
--a)
length' :: Num a => [a] -> a
length' = foldl (\acc _ -> acc +1) 0

--b)
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

--c)
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x acc -> if p x then True && acc else False) True

--d)
elem' :: Eq a => a -> [a] -> Bool
elem' p = foldr (\x acc -> (x == p) || acc) False

--e)
takewhile' :: (a -> Bool) -> [a] -> [a]
takewhile' p = foldr (\x acc -> if p x then x : acc else []) []


--A7-2 
--a)
--foo = (* 4) 

--b)
--foo = bar . bar 

--c) 
--foo  = $ == 1 . $ `mod` 2

--d)
--foo  = sqrt . $ 5 * . sum . flip take [1..50]

--e)
--foo'  =  $ "River" ++ . $ : "Plate" . head   

--A7-3
--a)
alleungeradenQuadrate :: Integer
alleungeradenQuadrate  = sum  . takeWhile (< 10000) $ map (^2) $ filter odd [1..]

--b)
chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
  | even n    = n : chain (n `div` 2)
  | odd n     = n : chain (n * 3 + 1)

alleChainsmit15 :: Int
alleChainsmit15 = length . filter(\x -> length x > 15) . map chain $ [1..100]