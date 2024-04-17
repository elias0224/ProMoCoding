
kopf :: String -> Char
kopf s = s!!0

ende :: String -> Char
ende c = last c


rest :: String -> String
rest xs = reverse(init (reverse xs))

start :: String -> String
start xs = reverse(take 1 (reverse xs))

rest2 :: String -> String
rest2 xs = drop 1 xs

start2 :: String -> String
start2 xs = init xs