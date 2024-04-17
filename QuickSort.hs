qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort small ++ mid ++ qsort big
            where
                small = [y | y <- xs, y < x ]
                mid   = [y | y <- xs, y == x] ++[x]
                big   = [y | y <- xs, y > x ]