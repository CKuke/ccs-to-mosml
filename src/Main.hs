
module Main (main) where

import System.Environment(getArgs)

import CCSMosml

main :: IO ()
main = do 
    (command : args) <- getArgs
    case command of
        "mosml" -> return ()
            -- let 
            --     out = "mosml/out.sml"
            --     (path:rest) = args
            -- in do
            --     ccsString <- readFile path
            --     let mosml = ccsToMosml ccsString
            --     writeFile out mosml
        _ -> do putStrLn("Command: " ++ command ++ " does not exist")
