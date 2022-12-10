{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
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
    (outs, env)  <- typecheckTerm' t
    env' <- withEnv env $ typecheckConds $ fromMaybe [] cs
    _ <- withEnv (env++env') $ typecheckTerms outs ts
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
    (outs, env) <- typecheckTerm' t
    env' <- withEnv env $ typecheckTerms outs ts
    return $ env++env'
    
    -- subSorts <- createSorts sort ts
    -- typecheckTerms subSorts ts




-- Like typcheckTerm except there is no expected sort.
-- If a term without a type is encountered then that is an error
-- Returns the sort of newly typed subterms
typecheckTerm' :: Term -> Typechecker ([Id], [Sort])
typecheckTerm' (Term id ts) = do
    sort <- lookupEnv id
    case sort of
      Nothing -> -- Sort error
        abort $ "Term " ++id++ " has no sort"
      Just so -> do
        -- check the expected types for subterms
        let (Sort _ ins outs) = so
        let ts' = fromMaybe [] ts
        let ins' = fromMaybe [] ins
        env <- typecheckTerms ins' ts'
        return (outs, env)



-- Recursively checks a list oif a list of terms has the corerct sorts
typecheckTerms :: [Id] -> [Term] -> Typechecker [Sort]
typecheckTerms (s:ss) (t:ts) = do
    env  <- typecheckTerm s t
    env' <- withEnv env $ typecheckTerms ss ts
    return $ env ++ env'
typecheckTerms [] ts = do if not (null ts) then abort "No" else return []
typecheckTerms ss [] = do if not (null ss) then abort "No" else return []




typecheckTerm :: Id -> Term -> Typechecker [Sort]
typecheckTerm expected (Term id Nothing) = do
    sortM <- lookupEnv id 
    case sortM of
        Nothing -> do -- Give the term a new sort
            let sort = Sort id Nothing [expected]
            return [sort]
        Just (Sort _ Nothing [actual]) -> -- Check if sorts match
            if actual == expected then return []
            else do 
                abort $ id++" expected to have sort "++expected++" but found "++actual
        Just (Sort _ _ _) -> 
            -- This case only exists to satisfy the compiler
            error "TypecheckTerm error 1"
typecheckTerm expected (Term id (Just ts)) = do
    sortM <- lookupEnv id
    case sortM of
        Nothing -> error "All terms with args should have a sort"
        Just (Sort _ Nothing _) -> error "Term should have in-set sorts"
        Just (Sort _ (Just ins) [actual]) -> do
            if expected /= actual then 
                abort $ id++" expected to have sort "++expected++" but found "++actual
            else do -- check all the subterms
                typecheckTerms ins ts
        Just (Sort _ _ _) -> 
            -- This case only exists to satisfy the compiler
            error "TypecheckTerm error 2"
                


-- Create the sorts for some subterms based on the Sort of
-- the parent term
createSorts :: Sort -> [Term] -> Typechecker [Sort]
createSorts (Sort _ Nothing _) _ = do return []
createSorts (Sort sid (Just ins) _) ts =
    return $ zipWith (\s (Term id _) -> Sort id Nothing [s]) ins ts









