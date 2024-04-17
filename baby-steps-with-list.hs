
kopf :: [a] -> a
kopf s = s!!0

ende :: [a] -> a
ende c = last c


rest :: [a] -> [a]
rest xs = drop 0 xs

start :: [a] -> [a]
start xs = init xs

