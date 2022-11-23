{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}


module CCSMosml where

import CCSAst
import Data.List
import Control.Applicative (Alternative(empty))

{-
This module will preprocess the AST and produce a list of Tasks that will then
be translated to the MosML source code.

Datatype values will be converted to MosML datatypes and Functions to MosML functions
-}


data Task =
    Datatype Id [Constructor]
    | Function Id [Rule]
    deriving (Eq, Read, Show)
{-
Defines a constructor of a datatype in MosML. The Id is the name of the 
constructor and the maybe id list is the constructor arguments. We need a
new datatype for this, as the only thing we have from the CCS AST is the sorts
and now we need to create some structure from them. 

datatype Nat
    | z           <-- Constructor   (Nothing)
    | s of Nat    <-- Constructor   (Just)
-}
data Constructor = Constructor Id (Maybe [Id])
    deriving (Eq, Read, Show)


toDatatype :: CCS -> [Task]
toDatatype (Ccs _ sigs sorts _) =
    -- find all constructor sorts
    let sorts' =
            let sigIds   = getSigIds sigs
                sortIds  = getSortIds sorts
                sortIds' = filter (`notElem` sigIds) sortIds
            in filter (\x-> let (Sort id _ _) = x in id `elem` sortIds') sorts

    -- find unique return sorts/types (datatype ids)
        dtIds = nub $ getReturnSort sorts'

    -- create the actual datatypes
    in toDatatype' dtIds sorts'

-- expects id list to contain unique ids
-- TODO: What happens if sorts is longer than ids?
toDatatype' :: [Id] -> [Sort] -> [Task]
toDatatype' [] _ = []
toDatatype' (id:ids) sorts =
    let (sorts', sorts'') = partition (\(Sort _ _ [ret]) -> ret == id) sorts
        const = map (\(Sort sid args _) -> Constructor sid args) sorts'
    in Datatype id const : toDatatype' ids sorts''




toFunction :: CCS -> [Task]
toFunction (Ccs _ sigs _ rules) =
    let sigIds = getSigIds sigs
    in toFunction' sigIds rules

toFunction' :: [Id] -> [Rule] -> [Task]
toFunction' [] _ = []
toFunction' (id:ids) rules =
    let (rules', rules'') = partition (\(Rule (Term rid _) _ _) -> rid == id) rules
        fun = Function id rules'
    in fun : toFunction' ids rules''


{-
Functions for conversion of different parts into MosML code
-}

taskToString :: [Task] -> String
taskToString [] = ""
taskToString (t:ts) =
    case t of
        Datatype id cons ->
            let header = "datatype " ++ id ++ " =\n"
                cons' = constructorsToStrings cons
            in header ++ "\t" ++ intercalate "\n\t| " cons'
        Function id rules ->
            let header = "fun "
                footer = "end"
            in header ++ footer


constructorsToStrings :: [Constructor] -> [String]
constructorsToStrings [] = empty
constructorsToStrings (c:cs) =
    case c of
        Constructor id Nothing -> id : constructorsToStrings cs
        Constructor id (Just args) ->
            let args' = intercalate " * " args
            in (id ++ " of " ++ args') : constructorsToStrings cs


-- rulesToStrings :: [Rule] -> [String]
-- rulesToStrings [] = empty
-- rulesToStrings (r:rs) =
--     let (Rule t1 ts conds) = r
--         pat = termToString t1
--         stmts =
--             case conds of
--                 Nothing -> ""
--                 (Just conds') -> 
--                     let stmts = condsToStrings conds'
--                     in "let " ++ intercalate "\n"



termToString :: Term -> String
termToString (Term id ts) =
    case ts of
        Nothing -> id
        Just ts' ->
            let tStrings = map ((\x-> if ' ' `elem` x then "("++x++")" else x) . termToString) ts'
            in id ++ " " ++ intercalate " " tStrings


condsToStrings :: [Cond] -> [String]
condsToStrings [] = empty
condsToStrings (c:cs) =
    let (Cond exp vals) = c
        vals' = case vals of
            [v] -> termToString v
            vs -> 
                let tmp = intercalate ", " $ map termToString vals
                in "("++tmp++")"
        exp' = termToString exp
    in ("val " ++ vals' ++ " = " ++ exp' ++ "\n") : condsToStrings cs

{-
Utility functions
-}

getReturnSort :: [Sort] -> [Id]
getReturnSort [] = []
getReturnSort (s:ss) =
    let (Sort _ _ [ret]) = s in
    ret : getReturnSort ss

getSortIds :: [Sort] -> [Id]
getSortIds [] = []
getSortIds (s:ss) =
    let (Sort id _ _) = s in
    id : getSortIds ss

getSigIds :: [Sig] -> [Id]
getSigIds [] = []
getSigIds (s:ss) =
    let (Sig id _ _) = s in
    id : getSigIds ss








