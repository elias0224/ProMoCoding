fac' :: Integer -> Integer
fac' 1 = 1
fac' x = x * fac' (x-1)

fac'' :: Integer -> Integer
fac'' 1 = 1
fac'' x = fac''' x 1
    where 
        fac''' x acc = acc * x * fac''(x-1)