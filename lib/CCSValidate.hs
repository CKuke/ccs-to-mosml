{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}


module CCSValidate where


import Data.List
import Control.Monad.Trans.RWS.Lazy

import CCSAst


{-
This module will contain functionality for validating a ccs. It will not test
for properties such as EV-Free, left-to-right deterministic and so on, but will
make sure that terms defined as variables are only used as variables, terms applied
as functions get the correct number of arguments and so on.
-}


{-
Monad with state bool and writeable state of type String.
The bool will be used to indicate whether an error has been encountered.
A warning will not set it to true.
-}
type Validator = RWS () [String] Bool ()

-- validate :: CCS -> Bool
-- validate ccs =
--     let (res, _) = evalRWS 


dupVars :: CCS -> Validator
dupVars (Ccs vars _ _ _) =
    let varIds = map (\(Var id) -> id) vars
        uniques = getDups varIds
        msgs = map (\x -> "warning dup VAR: " ++ x ++ "\n") uniques
    in do 
        tell msgs
        return ()

dupSigs :: CCS -> Validator
dupSigs (Ccs _ sigs _ _) =
    let sigIds = map (\(Sig id _ _) -> id) sigs
        uniques = getDups sigIds
        msgs = map (\x -> "error dup SIG: " ++ x) uniques
    in
        if null msgs then
            return ()
        else do
            tell msgs
            put True
            return ()

dupSorts :: CCS -> Validator
dupSorts (Ccs _ _ sorts _) =
    let sortIds = map (\(Sort id _ _) -> id) sorts
        uniques = getDups sortIds
        msgs = map (\x -> "error dup SORT: " ++ x) uniques
    in
        if null msgs then return ()
        else do
            tell msgs
            put True
            return () 

-- checks if any of the defined vars is also defined as either a sort or sig
overlap :: CCS -> Validator
overlap (Ccs vars sigs sorts _) =
    let varIds = map (\(Var id) -> id) vars
        sigIds = map (\(Sig id _ _) -> id) sigs
        sortIds = map (\(Sort id _ _) -> id) sorts

        varIds' = nub varIds
        sigIds' = nub sigIds
        sortIds' = nub sortIds

        varSig = varIds' `intersect` sigIds'
        varSort = varIds' `intersect` sortIds'

        msgs1 = map (\x -> "error VAR " ++ x ++ " also defined in SIG") varSig
        msgs2 = map (\x -> "error VAR " ++ x ++ " also defined in Sort") varSort
        msgs = msgs1 ++ msgs2

    in if null msgs then return ()
       else do
            tell msgs
            put True
            return ()

-- check that all rules are defined in SIG and SORT
-- Type check the rules
-- Is all variables in RULES defined in VAR
-- Is all non-variable terms in RULES defined in SIG and/or SORT


-- Utility function to get all duplicate ids from a list of ids
getDups :: [Id] -> [Id]
getDups src =
    let fun src seen res =
            case src of 
                [] -> res
                (id:ids) ->
                    if id `elem` seen && id `notElem` res then 
                        fun ids seen (id : res)
                    else if id `notElem` seen then 
                        fun ids (id : seen) res
                    else
                        fun ids seen res
    in fun src [] []

