
module Main (main) where

import System.Environment(getArgs)

import CCSMosml
import CCSParser (parseProgram)

main :: IO ()
main = do 
    (command : args) <- getArgs
    case command of
        "mosml" -> 
            let 
                out = "mosml/out.sml"
                (path:rest) = args
            in do
                ccsString <- readFile path
                case parseProgram ccsString of
                    Left err -> putStrLn err
                    Right ccs -> do
                        -- TODO: add some validation here
                        let mosml = translate ccs
                        writeFile out mosml
        _ -> do putStrLn("Command: " ++ command ++ " does not exist")
