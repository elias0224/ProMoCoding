
--A5-3
data Player = Player { name :: String , team :: String, nummer :: Int, foot :: Foot, position :: Position } deriving (Show, Eq)
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper  | Defender | Midfielder | Forward deriving (Show, Eq)

--A5-4
sameTeam :: [Player] -> Bool
sameTeam [] = True
sameTeam (x:xs) = all (\player -> team player == team x) xs

uniqueNumbers :: [Player] -> Bool
uniqueNumbers [] = True
uniqueNumbers (x:xs) = not (x `elem` xs) && uniqueNumbers xs

coverPosition :: [Player] -> Bool
coverPosition players = all (\pos -> pos `elem` map position players) [Goalkeeper, Defender, Midfielder, Forward]