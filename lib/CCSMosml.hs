{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
module CCSMosml where


import CCSAst
import Control.Monad.Trans.RWS.Lazy
import Data.List (intercalate)




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


type Builder = RWS [Task] () ()


translate :: [Task] -> String
translate tasks =
    let (a,_) = evalRWS tasksToString tasks ()
    in a


tasksToString :: Builder String
tasksToString = do
    tasks <- ask
    strings <- mapM taskToString tasks
    return ""


taskToString :: Task -> Builder String
taskToString task =
    case task of
        Datatype id sorts -> do
            let header = "datatype " ++ id ++ " =\n\t"
            constr <- mapM sortToString sorts
            return $ header ++ intercalate "\n\t|" constr
        Function id rules -> return ""


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
       




