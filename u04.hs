
--A4-1
palindrome :: String  -> Bool
palindrome []  = True
palindrome [_] = True
palindrome x
            | head x == last x      = palindrome (init(tail x))
            | otherwise             = False

--A4-2
sieve' ::  [Int] -> [Int]
sieve' [] = []
sieve' (p:xs) = p: sieve'[x | x <- xs, x `mod` p > 0]

--A4-3
--other file
--Quicksort.hs

--A4-4
foo x = x+1 
bar x = x*2
baz x y z = if x < y then foo z else bar z

--  a)    a = 2
--        a = 4
--  b)    b = 12
--        c = 18
--        baz 9 12 18 = if 9 < 12 then foo 18 else bar 18
--        foo 18 = 19
--        c = 19
--  c)    d = bar 5 = 10
--        bar 0 * foo 10
--        0 * 11
--        d = 0
