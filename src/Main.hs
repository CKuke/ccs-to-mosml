
module Main (main) where

import System.Environment(getArgs)
import Data.List
import Control.Monad

import CCSMosml
import CCSParser (parseProgram)
import CCSValidate
import CCSTypecheck

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
                        case validate ccs of
                            Just msgs -> putStrLn $ intercalate "\n" msgs
                            Nothing ->
                                case typecheck ccs of
                                    Just msgs -> putStrLn $ intercalate "\n" msgs
                                    Nothing ->
                                        let mosml = translate ccs in 
                                        writeFile out mosml

                        -- TODO: add some validation here
                        -- when (validate ccs) $ do
                        --     when (typecheck ccs) $ do
                        --             let mosml = translate ccs
                        --             writeFile out mosml
        _ -> do putStrLn("Command: " ++ command ++ " does not exist")
