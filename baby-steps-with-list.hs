
kopf :: String -> Char
kopf s = s!!0

ende :: String -> Char
ende c = kopf(reverse c)

rest :: String -> String
rest xs = reverse(take (length xs -1)(reverse xs))

start :: String -> String
start xs = reverse(drop 1 (reverse xs))

rest2 :: String -> String
rest2 xs = drop 1 xs

start2 :: String -> String
start2 xs = take (length xs -1) xs