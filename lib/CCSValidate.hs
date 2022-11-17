{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}


module CCSValidate (validateCCS) where


import Data.List

import CCSAst



validateCCS :: CCS -> Maybe ErrMsg
validateCCS (Ccs vars sigs sorts rules) =
    -- Check for dupliacte values on VAR, SIG and SORT
    let varIds  = getDuplicates $ map (\(Var id) -> id) vars
        sigIds  = getDuplicates $ map (\(Sig id _ _) -> id) sigs
        sortIds = getDuplicates $ map (\(Sort id _ _) -> id) sorts

        dupErrors = concat $ (map (\id -> "Duplicate VAR: " ++ id ++ "\n") varIds) ++
                          (map (\id -> "Duplicate SIG: " ++ id ++ "\n") sigIds) ++
                          (map (\id -> "Duplicate SORT: " ++ id ++ "\n") sortIds)

    -- Check that if id is in VAR then it is not in SIG and vice verca
        overlapErrors =
            let overlap = varIds `intersect` sigIds
            in concatMap (\id -> "Id " ++ id ++ " both in VAR and SIG" ++ "\n") overlap

    -- Is all variables in RULES defined in VARS
        varErrors =
            let ruleVars = getVarsFromRules rules
                vars' = map (\(Var id) -> id) vars
                missing = filter (`notElem` vars') ruleVars
            in concatMap (\id -> "Var " ++ id ++ " not defined\n") missing

    -- Is all non var terms in RULES defined in SIG or SORT
    -- Do all functions have the correct arity in RULES

    -- Do the variables have the correct types (type inference)

        errors = dupErrors ++ overlapErrors ++ varErrors
    in case errors of
      "" -> Nothing
      errs -> Just errs


getDuplicates :: [Id] -> [Id]
getDuplicates ids = ids \\ (nub ids)



-- Extract all variable terms from a list of rules
getVarsFromRules :: [Rule] -> [Id]
getVarsFromRules [] = []
getVarsFromRules (r:rs) =
    case r of
        Rule _ terms Nothing -> getVarsFromTerms terms ++ getVarsFromRules rs
        Rule _ terms (Just conds) ->
            let tmp = concatMap (\(Cond _ ts) -> ts) conds
            in getVarsFromTerms terms ++ getVarsFromTerms tmp ++ getVarsFromRules rs


getVarsFromTerms :: [Term] -> [Id]
getVarsFromTerms [] = []
getVarsFromTerms (t:ts) =
    case t of
        Term id Nothing -> id : (getVarsFromTerms ts)
        Term id (Just ts') -> getVarsFromTerms ts' ++ getVarsFromTerms ts
