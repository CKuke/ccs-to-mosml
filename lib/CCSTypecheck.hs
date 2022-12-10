module CCSTypecheck where


import CCSAst

-- import Control.Monad.Reader
import Data.List
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, maybeToList)
import Data.Either
import Control.Monad



-- data Typechecker a = ReaderT [Sort] (Either String a)

newtype Typechecker a = Typechecker {runTypechecker :: [Sort] -> Either String a}

instance Monad Typechecker where
  return a = Typechecker $ \_ -> Right a
  m >>= f = Typechecker $ \env->
    case runTypechecker m env of
      Left e1 -> Left e1
      Right a1 ->
        case runTypechecker (f a1) env of
          Left e2 -> Left e2
          Right a2 -> Right a2

-- You shouldn't need to modify these
instance Functor Typechecker where
  fmap = liftM
instance Applicative Typechecker where
  pure = return; (<*>) = ap

{-
Utility functions
-}

abort :: String -> Typechecker a
abort msg = Typechecker $ \_ -> Left msg

ask :: Typechecker [Sort]
ask = Typechecker $ \e -> Right e

-- Extends the env
withEnv :: [Sort] -> Typechecker a -> Typechecker a
withEnv env m = Typechecker $ \e -> runTypechecker m (env++e)

lookupEnv :: Id -> Typechecker (Maybe Sort)
lookupEnv id = do
    find (\(Sort sid _ _)->sid==id) <$> ask



{-
Main function
-}
typecheck :: CCS -> Maybe [String]
typecheck (Ccs _ _ sorts rules) = do
    let res = map (\r -> runTypechecker (typecheckRule r) sorts) rules 
    let ls = lefts res
    case ls of
        [] -> Nothing
        ls' -> Just ls'



{-
Type checking functions
-}


typecheckRule :: Rule -> Typechecker ()
typecheckRule (Rule t ts cs) = do
    sort <- typecheckTerm' t
    env <- typecheckConds $ fromMaybe [] cs
    outSorts <- createSorts sort ts
    _ <- withEnv env $ typecheckTerms outSorts ts
    return ()



typecheckConds :: [Cond] -> Typechecker [Sort]
typecheckConds [] = do return []
typecheckConds (c:cs) = do 
    env  <- typecheckCond c
    env' <- withEnv env $ typecheckConds cs
    return $ env++env'


-- Returns potential new sorts for the term list part 
typecheckCond :: Cond -> Typechecker [Sort]
typecheckCond (Cond t ts) = do
    sort <- typecheckTerm' t
    subSorts <- createSorts sort ts
    typecheckTerms subSorts ts




-- Like typcheckTerm except there is no expected sort.
-- If a term withut a type is encountered then that is an error
-- Returns the sort of the top level term
typecheckTerm' :: Term -> Typechecker Sort
typecheckTerm' (Term id ts) = do
    sort <- lookupEnv id
    case sort of
      Nothing -> -- Sort error
        abort $ "Term " ++id++ " has no sort"
      Just so -> do
        -- check the expected types for subterms
        let ts' = fromMaybe [] ts
        subSorts <- createSorts so ts'
        _ <- typecheckTerms subSorts ts'
        return so




-- Recursively checks a list oif a list of terms has the corerct sorts
typecheckTerms :: [Sort] -> [Term] -> Typechecker [Sort]
typecheckTerms (s:ss) (t:ts) = do
    env  <- typecheckTerm s t
    env' <- withEnv env $ typecheckTerms ss ts
    return $ env ++ env'
typecheckTerms [] ts = do if not (null ts) then abort "No" else return []
typecheckTerms ss [] = do if not (null ss) then abort "No" else return []




-- Returns the types for not yet seen variables
typecheckTerm :: Sort -> Term -> Typechecker [Sort]
typecheckTerm expected (Term id ts) = do
    let (Sort _ _ expected') = expected
    actualM <- lookupEnv id
    case actualM of
      Nothing -> do -- Give it the new sort and check subterms
        let ts' = fromMaybe [] ts
        subSorts <- createSorts expected ts'
        env' <- withEnv [expected] $ typecheckTerms subSorts ts'
        return $ expected : env'
      Just actual -> do -- Check if the sorts match
        let (Sort _ _ actual') = actual
        if actual' == expected' then do -- check potential subterms
            let ts' = fromMaybe [] ts
            subSorts <- createSorts actual ts'
            typecheckTerms subSorts ts'
        else do
            abort $ "Term " ++id++ " expected to have sort " ++show expected++
                    " but found " ++ show actual


-- Create the sorts for some subterms based on the Sort of
-- the parent term
createSorts :: Sort -> [Term] -> Typechecker [Sort]
createSorts (Sort _ Nothing _) _ = do return []
createSorts (Sort sid (Just ins) _) ts =
    return $ zipWith (\s (Term id _) -> Sort id Nothing [s]) ins ts
















-- type Typechecker a = Reader [Sort] (Either [String] a)


-- typecheck :: CCS -> Bool
-- typecheck (Ccs _ _ sorts rules) =
--     let res = map (\r -> runReader (typecheckRule r) sorts) rules
--         ls = (concat . lefts) res
--         rs = (concat . rights) res
--     in null ls || (
--         let _ = map print ls in
--         False)


-- typecheckRule :: Rule -> Typechecker [Sort]
-- typecheckRule (Rule t ts cs) = do
--     headerEnv <- typecheckTerm' t
--     case headerEnv of
--         (Right env') -> do
--             -- check conditions
--             env'' <- local (++env') $ typecheckConds (concat . maybeToList $ cs)
--             case env'' of
--                 (Right env''') -> do
--                     -- check the out set
--                     let tmpEnv = map (\(Sort _ _ [s]) -> s) env'
--                     tmp <- local (++env''') $ zipWithM typecheckTerm tmpEnv ts
--                     return $ Right tmp
--                 (Left errs) -> return $ Left errs
--         (Left errs) -> return $ Left errs
--     return $ Right []




-- typecheckConds :: [Cond] -> Typechecker [Sort]
-- typecheckConds [] = do asks Right
-- typecheckConds (c:cs) = do
--     let (Cond t ts) = c
--     env' <- typecheckTerm' t
--     case env' of
--         (Left errs) -> return $ Left errs
--         (Right env'') -> do
--             let tmpEnv = map (\(Sort _ _ [s]) -> s) env''
--             mEnv <- local (++ env'') $ zipWithM typecheckTerm tmpEnv ts
--             let ls = (concat . lefts) mEnv
--             let rs = (concat . rights) mEnv
--             if not (null ls) then do
--                 return $ Left ls
--             else do
--                 -- check remaining conditions
--                 local (\e-> e++rs) $ typecheckConds cs



-- -- like typecheckTerm but without expected type
-- typecheckTerm' :: Term -> Typechecker [Sort]
-- typecheckTerm' (Term id ts) = do
--     env <- ask
--     let maybeSort = find (\(Sort sid _ _)->sid==id) env
--     if isNothing maybeSort then
--         error "Should never happen. All top level terms should have sort"
--     else do
--         let (Sort _ inSorts outSorts) = fromJust maybeSort
--         let ts' = fromJust ts
--         let inSorts' = fromJust inSorts
--         tmp <- zipWithM typecheckTerm inSorts' ts'
--         let (ls, rs) =
--                 let (t1, t2) = partitionEithers tmp
--                 in (concat t1, concat t2)
--         if not (null ls) then do
--             return $ Left ls
--         else do
--             return $ Right rs



-- typecheckTerm :: Id -> Term -> Typechecker [Sort]
-- typecheckTerm expected (Term id Nothing) = do
--     env <- ask
--     let maybeSort = find (\(Sort sid _ _)->sid==id) env
--     if isNothing maybeSort then do
--         -- Give the sort to the term
--         return $ Right [Sort id Nothing [expected]]
--     else do
--         let (Sort _ _ [actual]) = fromJust maybeSort
--         let match = expected == actual
--         if match then do
--             return $ Right []
--         else do
--             return $ Left [
--                 "Term " ++ id ++ " expected to have sort " ++ expected ++
--                 " but has sort " ++ actual
--                 ]

-- typecheckTerm expected (Term id (Just ts)) = do
--     env <- ask
--     let (Just (Sort _ lhs [actual])) = find (\(Sort sid _ _)->sid==id) env
--     if expected /= actual then do
--         return $ Left ["Term " ++ id ++ " expected to have sort " ++ expected
--                         ++ " but has sort " ++ actual]
--     else do
--         let inSorts = fromMaybe [] lhs
--         sorts <- zipWithM typecheckTerm inSorts ts
--         let ls = lefts sorts
--         if not (null ls) then do
--             return $ Left $ concat ls
--         else do
--             let tmp = concat $ rights sorts
--             return $ Right tmp















