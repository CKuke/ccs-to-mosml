module Main (main) where

import System.Environment(getArgs)



main :: IO ()
main = do 
    (command : args) <- getArgs
    case command of
        "mosml" -> do putStrLn("Yes")
        _ -> do putStrLn("Command: " ++ command ++ " does not exist")
