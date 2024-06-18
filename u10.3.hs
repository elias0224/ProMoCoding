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
    


dictLookup :: [(String, String)] -> String -> [String]
dictLookup dict word = map snd $ filter ((==word) . fst) dict