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


-- The reader part (environment) is a list of all constructor ids such
-- as cons, succ, nil and so on.
type Builder = RWS [Id] () String


-- Returns the tasks to be translated and a list of all ids for constructors
ccsToTasks :: CCS -> ([Task], [Id])
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
        (dTypeTasks ++ funTasks, sortIds')






translate :: CCS -> String
translate ccs =
    let (tasks, cs) = ccsToTasks ccs
        (a,_) = evalRWS (tasksToString tasks) cs "fun  "
    in a


tasksToString :: [Task] -> Builder String
tasksToString tasks = do
    -- tasks <- ask
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
            header <- get
            pats <- mapM ruleToString rules
            let pats' = intercalate "\n\t|" pats
            put "and  "
            return $ header ++ pats'
            


sortToString :: Sort -> Builder String
sortToString (Sort id args _) =
    case args of
        Nothing -> return id
        Just args' ->
            return $ id ++ " of " ++ intercalate " * " args'


-- add(x, y) === add x y                function
-- cons(z, nil) == cons (z, nil)        constructor
termToString :: Term -> Builder String
termToString (Term id terms) = do
    cs <- ask
    case terms of
        Nothing -> return id
        (Just ts) -> 
            if id `elem` cs then do
                ts' <- mapM termToString ts
                let ts'' = "(" ++ intercalate ", " ts' ++ ")"
                return $  id ++ " " ++ ts''
            else do
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
       let ts'' = 
            let tmp = intercalate ", " ts'
            in if ',' `elem` tmp then "("++tmp++")" else tmp
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
            do ret  <- mapM termToString ts
               let ret' = intercalate ", " ret
               if ',' `elem` ret' then
                    return $ "("++ret'++")"
               else 
                    return ret'
       stmts <-
            case conds of
                Nothing -> return ""
                Just conds' ->
                    do ss <- mapM condTostring conds'
                       return $ "let " ++ intercalate "; " ss ++ " in "
       let end = if null stmts then "" else " end"
       return $ pat ++ " = " ++ stmts ++ ret ++ end
       




