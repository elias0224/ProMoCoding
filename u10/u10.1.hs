import System.IO
import Data.Char



main :: IO()
main = do 
    putStrLn "Wählen sie einen Gesangsstil:"
    stil <- getLine
    putStrLn "Wie ist ihr Name?"
    name <- getLine
    content <- readFile "one-hit-wonder.txt"
    let changedName = changeName name content
    putStr "Hier ist ihr Lied:"
    if(stil == "laut")
        then putStrLn (map toUpper changedName)
        else if ("leise" == stil)
            then putStrLn (map toLower changedName)
            else putStrLn changedName
    

changeName :: String -> String -> String
changeName name content = unlines (map((unwords . wordsToName name "M") . words)(lines content))

wordsToName :: String -> String -> [String] -> [String]
wordsToName _ _ []                           = []
wordsToName name word (x:xs) | (x == word)   = name : wordsToName name word xs
                             | otherwise     = x:wordsToName name word xs
