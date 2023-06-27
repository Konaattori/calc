module Main where
import Parser (calc)
import Data.Char (toUpper)

main::IO()
main = do
    putStrLn "Enter a mathematical expression  eg. 2/(2+3)*4.33 - -6"
    loop

loop::IO()
loop = do
    str <- getLine
    if map toUpper str `elem` ["Q","QUIT"] then putStrLn "exiting..." else
        do
        let calculated = calc str
        case calculated of
            Just value -> do
                print value
                loop
            Nothing -> do
                putStrLn "Incorrect input"
                loop