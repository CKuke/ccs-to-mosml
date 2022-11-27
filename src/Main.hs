
module Main (main) where

import System.Environment(getArgs)
import Data.List

import CCSMosml
import CCSParser (parseProgram)

main :: IO ()
main = do
    (command : args) <- getArgs
    case command of
        "mosml" ->
            let
                (path:rest) = args
                out =
                    let (Just ind1) = elemIndex '.' path -- might fail
                        ind2 = last $ elemIndices '/' path
                        (_, name) = 
                            let (tmp, _) = splitAt ind1 path
                            in splitAt ind2 tmp
                    in "mosml/" ++ name ++ ".sml"
            in do
                ccsString <- readFile path
                case parseProgram ccsString of
                    Left err -> putStrLn err
                    Right ccs -> do
                        -- TODO: add some validation here
                        let mosml = translate ccs
                        writeFile out mosml
        _ -> do putStrLn("Command: " ++ command ++ " does not exist")
