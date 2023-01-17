{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}


module CCSValidate where


import Data.List
import Control.Monad.Trans.RWS.Lazy

import CCSAst
import Control.Monad
import Data.Maybe


{-
This module contains functionality for validating a ccs. It will not test
for properties such as EV-Free, left-to-right deterministic and so on, but will
make sure that terms defined as variables are only used as variables, terms applied
as functions get the correct number of arguments and so on.
-}



-- When validating we can encounter either errors or warnings. 
-- It is possible to continue from errors 
data ValidationMsg =
      Error String
    | Warning String
    deriving(Eq, Read, Show)

{-
Monad with state bool and writeable state of type String.
The bool will be used to indicate whether an error has been encountered.
A warning will not set it to true.
-}
type Validator = RWS () [ValidationMsg] Bool ()


-- will maybe return error messages
validate :: CCS -> Maybe [String]
validate ccs =
    let (_, out) = execRWS (validate' ccs) () False
    in case out of
      [] -> Nothing
      msgs -> Just $ map show msgs


validate' :: CCS -> Validator
validate' ccs = do
    dupVars ccs
    dupSigs ccs
    dupSorts ccs
    overlap ccs
    s1 <- get
    unless s1 $ do
        rules ccs
        checkVars ccs
        checkNonVars ccs
        checkArities ccs
        -- s2 <- get
        -- unless s2 $ do
        --     typecheck ccs


dupVars :: CCS -> Validator
dupVars (Ccs vars _ _ _) = do
    let varIds = map (\(Var id) -> id) vars
    let uniques = getDups varIds
    let msgs = map (\x -> Warning $ "dup VAR: " ++ x ++ "\n") uniques
    tell msgs

dupSigs :: CCS -> Validator
dupSigs (Ccs _ sigs _ _) = do
    let sigIds = map (\(Sig id _ _) -> id) sigs
    let uniques = getDups sigIds
    let msgs = map (\x -> Error $ "dup SIG: " ++ x) uniques
    tell msgs
    unless (null msgs) (put True)

dupSorts :: CCS -> Validator
dupSorts (Ccs _ _ sorts _) = do
    let sortIds = map (\(Sort id _ _) -> id) sorts
    let uniques = getDups sortIds
    let msgs = map (\x -> Error $ "dup SORT: " ++ x) uniques
    tell msgs
    unless (null msgs) (put True)

-- checks if any of the defined vars is also defined as either a sort or sig
overlap :: CCS -> Validator
overlap (Ccs vars sigs sorts _) = do
    let varIds = map (\(Var id) -> id) vars
    let sigIds = map (\(Sig id _ _) -> id) sigs
    let sortIds = map (\(Sort id _ _) -> id) sorts

    let varIds' = nub varIds
    let sigIds' = nub sigIds
    let sortIds' = nub sortIds

    let varSig = varIds' `intersect` sigIds'
    let varSort = varIds' `intersect` sortIds'

    let msgs1 = map (\x -> Error $ "VAR " ++ x ++ " also defined in SIG") varSig
    let msgs2 = map (\x -> Error $ "VAR " ++ x ++ " also defined in Sort") varSort
    let msgs = msgs1 ++ msgs2
    tell msgs
    unless (null msgs) (put True)


-- check that all rules are defined in SIG and SORT
rules :: CCS -> Validator
rules (Ccs _ sigs sorts rules) = do
    let sigIds  = map (\(Sig id _ _) -> id) sigs
    let sortIds = map (\(Sort id _ _) -> id) sorts
    let ruleIds = map (\(Rule (Term id _) _ _) -> id) rules

    let noSigs  = filter (`notElem` sigIds) ruleIds
    let noSorts = filter (`notElem` sortIds) ruleIds

    let msgs1 = map (\x -> Error $ "RULE: " ++ x ++ "has no SIG") noSigs
    let msgs2 = map (\x -> Error $ "RULE: " ++ x ++ "has no SORT") noSorts
    let msgs = msgs1 ++ msgs2

    tell msgs
    unless (null msgs) (put True)


-- Is all variables in RULES defined in VAR
checkVars :: CCS -> Validator
checkVars (Ccs vars _ sorts rules) = do
    let varIds = map (\(Var id) -> id) vars
    let usedIds = concatMap getIdsR rules
    let missing = filter (`notElem` varIds) usedIds
    let msgs = map (\x-> Error $ "RULE: var " ++ x ++ " not defined in VAR") missing
    tell msgs
    unless (null msgs) (put True)
    where
        getIdsT (Term id ts) =
            case ts of
                Nothing ->
                    -- only return the id if does not have a sort as then
                    -- it is a "constructor"
                    ([id | not (any (\(Sort sid _ _)-> sid==id) sorts)])
                Just ts' -> concatMap getIdsT ts'
        getIdsR (Rule t1 ts cs) =
            let id1 = getIdsT t1
                ids = concatMap getIdsT ts
                idsc = case cs of
                        Nothing -> []
                        Just cs' ->
                            concatMap (\(Cond t ts') -> getIdsT t ++ (concatMap getIdsT ts')) cs'
            in id1 ++ ids ++ idsc

-- Is all non-var terms in rules defined in sig and/or sort
checkNonVars :: CCS -> Validator
checkNonVars (Ccs _ sigs sorts rules) = do
    let sigIds = map (\(Sig id _ _) -> id) sigs
    let sortIds = map (\(Sort id _ _) -> id) sorts
    let usedIds = concatMap getIdsR rules
    let missing = filter (`notElem` (sigIds++sortIds)) usedIds
    let msgs = map (\x -> Error $ "RULE: term " ++ x ++ " not defined in SIG and/or SORT") missing
    tell msgs
    unless (null msgs) (put True)
    where
        getIdsT (Term id ts) =
            case ts of
                Nothing -> []
                Just ts' -> concatMap getIdsT ts'
        getIdsR (Rule t1 ts cs) =
            let ids1 = getIdsT t1
                ids2 = concatMap getIdsT ts
                idsc = case cs of
                    Nothing -> []
                    Just cs' ->
                        concatMap (\(Cond t ts') -> getIdsT t ++ (concatMap getIdsT ts')) cs'
            in ids1 ++ ids2 ++ idsc



checkArities :: CCS -> Validator
checkArities (Ccs _ sigs sorts rules) =
    let msgs = map (checkArityRule sigs sorts) rules
    in unless (null msgs) (put True)


checkArityRule :: [Sig] -> [Sort] -> Rule -> Validator
checkArityRule sigs sorts (Rule (Term id ts) ts2 cs) = do
    let arity = maybe 0 length ts
    let coarity = length ts2
    -- At this point it should have been validated that all rules has 
    -- one and only one sig and sort
    let [sig] = filter (\(Sig sid _ _) -> sid == id) sigs
    let [sort] = filter (\(Sort sid _ _) -> sid == id) sorts

    let sortArity   = let (Sort _ ids _) = sort in maybe 0 length ids
    let sortCoarity = let (Sort _ _ ids) = sort in length ids

    let msgs =
            let (Sig _ a c) = sig
                msg1 = ([Error $ "RULE " ++ id ++ " with arity " ++ show arity ++ " used on " ++ show a ++ "arguments" | a /= arity])
                msg2 = ([Error $ "RULE " ++ id ++ " with coarity " ++ show coarity ++ " returns " ++ show c ++ " values" | c /= coarity])
                msg3 = ([Error $ "RULE " ++ id ++ " has differing arity in SIG and SORT with " ++ show arity ++" and " ++ show sortArity | arity /= sortArity])
                msg4 = ([Error $ "RULE " ++ id ++ " has differing coarity in SIG and SORT with " ++ show coarity ++" and " ++ show sortCoarity | coarity /= sortCoarity])
            in msg1 ++ msg2 ++ msg3 ++ msg4
    tell msgs
    unless (null msgs) (put True)





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







