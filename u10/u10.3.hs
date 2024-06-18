import System.IO


main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    contentsWB <- readFile "u10.woerterbuch.txt"
    let dict = map read (lines contentsWB)

    putStr "Geben sie ein hochdeutsches Wort ein "
    word <- getLine

    let translation = dictLookup dict word
    if  null translation
        then putStrLn word
        else putStr $ unlines translation

--b)
-- main :: IO()
-- main = do
--     hSetBuffering stdout NoBuffering
--     contentsWB <- readFile "u10.woerterbuch.txt"
--     let dict = map read (lines contentsWB)

--     original <- readFile "original.txt"
--     putStrln $ unword $ map (look dict) (words original)

-- let translation = dictLookup dict word
--     if  null translation
--         then putStrLn word
--         else putStr $ unlines translation



-- look :: [(Sring, String)] -> String -> String
-- look dict word = case dictLookup dict word of 
--     (x:xs) -> xs 
--      _     -> word 

dictLookup :: [(String, String)] -> String -> [String]
dictLookup dict word = map snd $ filter ((==word) . fst) dict