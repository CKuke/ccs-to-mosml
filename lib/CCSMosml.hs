{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
module CCSMosml where


import CCSAst
import Control.Monad.Trans.RWS.Lazy
import Data.List




{-
To make translation from the CCS AST to MosML easier we will first transform
the AST into a new datatype which more closely represents MosML with all of
the uneeded data of the AST removed. This is especially useful when translating
MosML datatypes as these has to be created from the SORT information alone, which
at the current state does not represent a MosML datatype very well
-}


data Task =
      Datatype Id [Sort]
    | Function Id [Rule]
    deriving(Eq, Show)


type Builder = RWS [Task] () ()


ccsToTasks :: CCS -> [Task]
ccsToTasks (Ccs _ sigs sorts rules) =
    let 
        -- Find the sorts describing MosML datatypes
        sigIds   = map (\(Sig id _ _) -> id) sigs
        sortIds  = map (\(Sort id _ _) -> id) sorts
        sortIds' = filter (`notElem` sigIds) sortIds
        sorts'   = filter (\s -> let (Sort id _ _) = s in id `elem` sortIds') sorts
        
        -- Find how many unique datatypes that will be created
        findTypes ss = -- function
            case ss of 
                [] -> []
                (s:ss') -> let (Sort _ _ [ret]) = s 
                           in ret : findTypes ss'
        dTypeIds = nub $ findTypes sorts'
        
        -- Create the Datatype tasks
        toDType ids ss =
            case (ids, ss) of
                ([], _) -> []
                (id:ids', ss) ->
                    let (ss', ss'') = partition (\(Sort _ _ [ret]) -> ret == id) ss
                    in Datatype id ss' : toDType ids' ss''
        dTypeTasks = toDType dTypeIds sorts'

        -- Create the function tasks
        toFunction ids rs =
            case (ids, rs) of
                ([], _) -> []
                (id:ids, rs) ->
                    let (rs', rs'') = partition (\(Rule (Term rid _) _ _) -> rid==id) rs
                    in Function id rs' : toFunction ids rs''
        funTasks = toFunction sigIds rules
    in 
        dTypeTasks ++ funTasks






translate :: CCS -> String
translate ccs =
    let tasks = ccsToTasks ccs
        (a,_) = evalRWS tasksToString tasks ()
    in a


tasksToString :: Builder String
tasksToString = do
    tasks <- ask
    strings <- mapM taskToString tasks
    return $ intercalate "\n\n" strings


taskToString :: Task -> Builder String
taskToString task =
    case task of
        Datatype id sorts -> do
            let header = "datatype " ++ id ++ " =\n\t"
            constr <- mapM sortToString sorts
            return $ header ++ intercalate "\n\t|" constr
        Function id rules -> do
            let header = "fun "
            pats <- mapM ruleToString rules
            let pats' = intercalate "\n\t|" pats
            return $ "fun " ++ pats' ++ "\nend"
            


sortToString :: Sort -> Builder String
sortToString (Sort id args _) =
    case args of
        Nothing -> return id
        Just args' ->
            return $ id ++ " of " ++ intercalate " * " args'


-- add(x, y) === add x y
termToString :: Term -> Builder String
termToString (Term id terms) =
    case terms of
        Nothing -> return id
        (Just ts)  -> do
            ts' <- mapM termToString ts
            let p x = if ' ' `elem` x then "("++x++")" else x
            let ts'' = intercalate " " $ map p ts'
            return $  id ++ " " ++ ts''

-- add(x, y) -> <z> === val z = add x y
-- add(s(x), y) -> <s(z)> === val s z = add (s x) y
condTostring :: Cond -> Builder String
condTostring (Cond t1 ts) =
    do t1' <- termToString t1
       ts' <- mapM termToString ts
       let ts'' = intercalate " " ts'
       return $ "val " ++ ts'' ++ " = " ++ t1'

-- add(z,y) -> <y> ===
--      add z y = y
-- add(s(x), y) -> <s(a)> <= add(x,y) -> <a> ===
--      add (s x) y =
--          let val a = add x y
--          in s z
ruleToString :: Rule -> Builder String
ruleToString (Rule t1 ts conds) =
    do pat <- termToString t1 
       ret <- 
            do ret <- mapM termToString ts
               return $ intercalate " " ret
       stmts <-
            case conds of
                Nothing -> return ""
                Just conds' ->
                    do ss <- mapM condTostring conds'
                       return $ "let " ++ intercalate "; " ss ++ " in "
       return $ pat ++ " = " ++ stmts ++ ret
       




