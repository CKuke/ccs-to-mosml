module CCSTypecheck where


import CCSAst

import Control.Monad.Reader
import Data.List
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, maybeToList)
import Data.Either



type Typechecker a = Reader [Sort] (Either [String] a)


typecheck :: CCS -> Bool
typecheck (Ccs _ _ sorts rules) =
    let res = map (\r -> runReader (typecheckRule r) sorts) rules
        ls = (concat . lefts) res
        rs = (concat . rights) res
    in null ls || (
        let _ = map print ls in
        False)


typecheckRule :: Rule -> Typechecker [Sort]
typecheckRule (Rule t ts cs) = do
    headerEnv <- typecheckTerm' t
    case headerEnv of
        (Right env') -> do
            -- check conditions
            env'' <- local (++env') $ typecheckConds (concat . maybeToList $ cs)
            case env'' of
                (Right env''') -> do
                    -- check the out set
                    let tmpEnv = map (\(Sort _ _ [s]) -> s) env'
                    tmp <- local (++env''') $ zipWithM typecheckTerm tmpEnv ts
                    return $ Right tmp
                (Left errs) -> return $ Left errs
        (Left errs) -> return $ Left errs
    return $ Right []




typecheckConds :: [Cond] -> Typechecker [Sort]
typecheckConds [] = do asks Right
typecheckConds (c:cs) = do
    let (Cond t ts) = c
    env' <- typecheckTerm' t
    case env' of
        (Left errs) -> return $ Left errs
        (Right env'') -> do
            let tmpEnv = map (\(Sort _ _ [s]) -> s) env''
            mEnv <- local (++ env'') $ zipWithM typecheckTerm tmpEnv ts
            let ls = (concat . lefts) mEnv
            let rs = (concat . rights) mEnv
            if not (null ls) then do
                return $ Left ls
            else do
                -- check remaining conditions
                local (\e-> e++rs) $ typecheckConds cs



-- like typecheckTerm but without expected type
typecheckTerm' :: Term -> Typechecker [Sort]
typecheckTerm' (Term id ts) = do
    env <- ask
    let maybeSort = find (\(Sort sid _ _)->sid==id) env
    if isNothing maybeSort then
        error "Should never happen. All top level terms should have sort"
    else do
        let (Sort _ inSorts outSorts) = fromJust maybeSort
        let ts' = fromJust ts
        let inSorts' = fromJust inSorts
        tmp <- zipWithM typecheckTerm inSorts' ts'
        let (ls, rs) =
                let (t1, t2) = partitionEithers tmp
                in (concat t1, concat t2)
        if not (null ls) then do
            return $ Left ls
        else do
            return $ Right rs



typecheckTerm :: Id -> Term -> Typechecker [Sort]
typecheckTerm expected (Term id Nothing) = do
    env <- ask
    let maybeSort = find (\(Sort sid _ _)->sid==id) env
    if isNothing maybeSort then do
        -- Give the sort to the term
        return $ Right [Sort id Nothing [expected]]
    else do
        let (Sort _ _ [actual]) = fromJust maybeSort
        let match = expected == actual
        if match then do
            return $ Right []
        else do
            return $ Left [
                "Term " ++ id ++ " expected to have sort " ++ expected ++
                " but has sort " ++ actual
                ]

typecheckTerm expected (Term id (Just ts)) = do
    env <- ask
    let (Just (Sort _ lhs [actual])) = find (\(Sort sid _ _)->sid==id) env
    if expected /= actual then do
        return $ Left ["Term " ++ id ++ " expected to have sort " ++ expected
                        ++ " but has sort " ++ actual]
    else do
        let inSorts = fromMaybe [] lhs
        sorts <- zipWithM typecheckTerm inSorts ts
        let ls = lefts sorts
        if not (null ls) then do
            return $ Left $ concat ls
        else do
            let tmp = concat $ rights sorts
            return $ Right tmp















