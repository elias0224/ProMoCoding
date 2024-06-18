import System.IO

main :: IO()
main = do 
    --hSetBuffering stdout NoBuffering (if you use putStr and not putStrLn)
    putStrLn "Deutsches Wort:"
    dword <- getLine
    if(null dword) 
        then do 
        putStrLn "Auf Wiedersehen"
        return ()
    else do 
        putStrLn "Bayerische Wort:"
        bword <- getLine
        appendFile "/CodingUniversity/ProMoCoding/ProMoCoding/u10a.woerterbuch.txt" $ show (dword, bword)
        putStrLn("'" ++ dword ++ "' heißt auf Bayerisch '" ++ bword ++ "'")
        main