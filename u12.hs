--A12-1
import Control.Monad
import Data.Fixed (mod')

tellOp :: (Show a, Show b) => (a -> b) -> a -> IO b
tellOp f x = let fx = f x in do
    putStrLn $ (show x) ++ " -> " ++ (show fx)
    return fx


test :: [Int -> IO Int]
test = map tellOp [ (*3), (+1), ( mod 7), (+5), (*2) ]

--a)
chainAction1 :: Monad m => a -> [(a -> m a)] -> m a
chainAction1 xs []     = return xs
chainAction1 xs (f:fs) = do
    x <- f xs
    chainAction1 x fs

--b)
chainAction2 :: Monad m => a -> [(a -> m a)] -> m a
chainAction2 xs [] = return xs
chainAction2 xs (f:fs) = f xs >>= (\y -> chainAction2 y fs)

--c)
chainAction3 :: Monad m => a -> [(a -> m a)] -> m a
chainAction3 xs l = foldM (\xs f -> f xs) xs l

--A12-2

data Logger a = Logger a [String]

instance (Show a) => Show (Logger a) where
             show (Logger v logs) = show v ++ "\n" ++ unlines (reverse logs)

--a)
-- instance Functor Logger where
--     fmap :: (a -> b) -> Logger a -> Logger b
--     fmap f (Logger x xs) =  Logger (f x) ((show f ): xs)


-- instance Applicative Logger where
--   pure :: a -> Logger a
--   pure a = Logger a []
--   (<*>) :: Logger (a -> b) -> Logger a -> Logger b
--   (<*>) (Logger x xs) (Logger y ys) = Logger (x y) (xs++ys) 

-- instance Monad Logger where
--   return = pure
--   (>>=) :: Logger a -> (a -> Logger b) -> Logger b
--   (>>=) (Logger x xs) f = f x

--b)

data Match = Match { homeTeam :: String -- Name of home team
                              , awayTeam :: String -- Name of away team
                              , homeScore :: Int   -- Goals scored by home team
                              , awayScore :: Int   -- Goals scored by away team
                              }
instance Show Match where
    show m = home ++ " - " ++ away
        where home = homeTeam m ++ " " ++ show (homeScore m)
              away = show (awayScore m) ++ " " ++ awayTeam m

-- Given two teams (h and a), start the match
startMatch :: String -> String -> Logger Match
startMatch h a = Logger (Match h a 0 0) ["Start!"]


-- Finish the current match
endMatch :: Match -> Logger Match
endMatch m = Logger m ["Ende"]


-- Given name of player (p) and minute of game (t), add one goal to home team
scoreHome :: String -> Int -> Match -> Logger Match
scoreHome p t m = Logger (Match (homeTeam m) (awayTeam m) (homeScore m+1) (awayScore m)) ["Goal! von " ++ p ++ " zum Zeitpunkt " ++ show t]


-- Given name of player (p) and minute of game (t), add one goal to away team
scoreAway :: String -> Int -> Match -> Logger Match
scoreAway p t m = Logger (Match (homeTeam m) (awayTeam m) (homeScore m) (awayScore m+1)) ["Goal! von " ++ p ++ " zum Zeitpunkt " ++ show t]

--c)

-- ex1 = 
--     startMatch "ARG" "FRA" >>= scoreHome "Messi" 23 >>= scoreHome "Di Maria" 36 >>= scoreAway "Mbappè" 80 >>= scoreAway "Mbappè" 81 >>= scoreHome "Messi" 108 >>= scoreAway "Mbappè" 118 >>= endMatch

--A12-3

type Knightpos = (Int,Int)

moveKnight :: Knightpos -> [Knightpos]
moveKnight (x,y) | checkpos (x,y)   = filter checkpos [(x+c, y+z*((c `mod` 2)+1))| c <- [-2,-1,1,2], z <- [-1,1]]
                 |otherwise         = error "Ungültige Position"
        where
            checkpos::Knightpos -> Bool
            checkpos (x,y) = x<9 && x>0 && y<9 && y>0

moveKnight2 :: [Knightpos] -> [Knightpos]
moveKnight2 []                            = []
moveKnight2 ((x,y):xs) | checkpos (x,y)   = filter checkpos [(x+c, y+z*((c `mod` 2)+1))| c <- [-2,-1,1,2], z <- [-1,1]] ++ moveKnight2 xs
                       |otherwise         = error "Ungültige Position"
        where
            checkpos::Knightpos -> Bool
            checkpos (x,y) = x<9 && x>0 && y<9 && y>0

in3Moves :: Knightpos -> [Knightpos]
in3Moves (x,y) = in3Moves' (in3Moves' (moveKnight (x,y)))
            where
                in3Moves':: [Knightpos] -> [Knightpos]
                in3Moves' []     = []
                in3Moves' (x:xs) = moveKnight x ++ in3Moves' xs

reachin3Moves :: Knightpos -> Knightpos -> Bool
reachin3Moves x y = x `elem` in3Moves y

inXMoves ::Int -> Knightpos -> [Knightpos]
inXMoves x y |  x>=2    = moveKnight2 (inXMoves (x-1) y)
             |otherwise = moveKnight y