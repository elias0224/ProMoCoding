
kopf :: String -> Char
kopf s = s!!0

ende :: String -> Char
ende c = last c


rest :: String -> String
rest xs = reverse(init (reverse xs))

start :: String -> String
start xs = reverse(drop 1 (reverse xs))

