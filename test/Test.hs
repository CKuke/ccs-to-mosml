{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Reader

import CCSAst
import CCSParser
import CCSValidate
import CCSMosml 
import CCSTypecheck 

parserTest = testGroup "Parser tests"
    [
        testGroup "VAR"
        [
            testCase "empty 1" $
                runParser parseVAR () "" "(VAR)"
                @?= Right [],
            testCase "empty 2" $
                runParser parseVAR () "" "(VAR )"
                @?= Right [],
            testCase "var 1" $
                runParser parseVAR () "" "(VAR a )"
                @?= Right [Var "a"],
            testCase "var 2" $
                runParser parseVAR () "" "(VAR a b)"
                @?= Right [Var "a", Var "b"]
        ],
        testGroup "SIG"
        [
            testCase "single fun" $
                runParser parseSig () "" "(add 2 1)"
                @?= Right (Sig "add" 2 1),
            testCase "multiple fun" $
                runParser parseSIG () "" "(SIG (add 2 1)\n(sub 42 90) )"
                @?= Right [Sig "add" 2 1, Sig "sub" 42 90]
        ],
        testGroup "SORT"
        [
            testCase "single sort" $
                runParser parseSORT () "" "(SORT (succ Nat -> Nat))"
                @?= Right [Sort "succ" (Just ["Nat"]) ["Nat"]],
            testCase "base case sort" $
                runParser parseSORT () "" "(SORT (nil -> List))"
                @?= Right [Sort "nil" Nothing ["List"]],
            testCase "unary" $
                runParser parseSORT () "" "(SORT (z -> Nat)\n(succ Nat -> Nat))"
                @?= Right [Sort "z" Nothing ["Nat"], Sort "succ" (Just ["Nat"]) ["Nat"]]
        ],
        testGroup "RULE"
        [
            testCase "term 1" $
                runParser parseTerm () "" "ident"
                @?= Right (Term "ident" Nothing),
            testCase "term 2" $
                runParser parseTerm () "" "ident(a, b)"
                @?= Right (Term "ident" (Just [Term "a" Nothing, Term "b" Nothing])),
            testCase "termlist 1" $
                runParser parseTermList () "" "a"
                @?= Right [Term "a" Nothing],
            testCase "termlist 2" $
                runParser parseTermList () "" "a, b, c"
                @?= Right [Term "a" Nothing, Term "b" Nothing, Term "c" Nothing],
            testCase "single cond" $
                runParser parseCond () "" "add(a, b) -> <c>"
                @?= Right (Cond
                    (Term "add" (Just [Term "a" Nothing, Term "b" Nothing]))
                    [Term "c" Nothing]),
            testCase "single cond more output" $
                runParser parseCond () "" "add(a, b) -> <x, s(y)>"
                @?= Right (Cond
                    (Term "add" (Just [Term "a" Nothing, Term "b" Nothing]))
                    [Term "x" Nothing, Term "s" (Just [Term "y" Nothing])]),
            testCase "cond conjunction" $
                runParser parseCondList () "" "add(a,b) -> <c> ^ sub(x, y) -> <z>"
                @?= Right ([
                    Cond (Term "add" (Just [Term "a" Nothing, Term "b" Nothing])) [Term "c" Nothing],
                    Cond (Term "sub" (Just [Term "x" Nothing, Term "y" Nothing])) [Term "z" Nothing]
                    ]),
            testCase "conds non empty" $
                runParser parseConds () "" "<= add(a, b) -> <c>"
                @?= Right ( Just
                    [Cond (Term "add" (Just [Term "a" Nothing, Term "b" Nothing])) [Term "c" Nothing]]
                    ),
            testCase "conds empty" $
                runParser parseConds () "" "empty"
                @?= Right Nothing,
            testCase "single rule" $
                runParser parseRuleDef () "" "l -> <r>"
                @?= Right (Rule (Term "l" Nothing) [Term "r" Nothing] Nothing),
            testCase "single rule with cond" $
                runParser parseRuleDef () "" "l -> <r> <= a -> <b>"
                @?= Right (Rule (Term "l" Nothing) [Term "r" Nothing] (
                        Just [Cond (Term "a" Nothing) [Term "b" Nothing]])
                    ),
            testCase "two rules" $
                runParser parseRuleList () "" "a -> <b>\n x -> <y>"
                @?= Right ([
                    Rule (Term "a" Nothing) [Term "b" Nothing] Nothing,
                    Rule (Term "x" Nothing) [Term "y" Nothing] Nothing
                    ]),
            testCase "two rules whith RULES" $
                runParser parseRULES () "" "(RULES a -> <b>\n x -> <y>)"
                @?= Right [
                    Rule (Term "a" Nothing) [Term "b" Nothing] Nothing,
                    Rule (Term "x" Nothing) [Term "y" Nothing] Nothing
                    ],
            testCase "add example rule" $
                let res = runParser parseRuleDef () "" "add(succ(x), y) -> <succ(z)> <= add(x,y) -> <z>"
                    left = Term "add" (Just [Term "succ" (Just [Term "x" Nothing]), Term "y" Nothing])
                    right = Term "succ" (Just [Term "z" Nothing])
                    cleft = Term "add" (Just [Term "x" Nothing, Term "y" Nothing])
                    cright = Term "z" Nothing
                in
                    res @?=
                    Right (Rule left [right] (Just [Cond cleft [cright]]))
        ],
        -- testGroup "COMMENT" 
        -- [

        -- ],
        testGroup "CCS"
        [
            testCase "add" $ do
                tmp <- (parseProgram <$> readFile "examples/add.ccs")
                let var = [Var "x", Var "y", Var "q"]
                    sig = [Sig "add" 2 1]
                    sort = [
                        Sort "z" Nothing ["Nat"],
                        Sort "succ" (Just ["Nat"]) ["Nat"],
                        Sort "add" (Just ["Nat", "Nat"]) ["Nat"]
                        ]
                    rules = [
                        Rule
                            (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                            ([Term "y" Nothing])
                            Nothing
                        ,Rule
                            (Term "add" (Just [Term "succ"(Just [Term "x" Nothing]), Term "y" Nothing]))
                            ([Term "succ" (Just [Term "q" Nothing])])
                            (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) [Term "q" Nothing]])
                        ]
                    in tmp @?= Right (Ccs var sig sort rules)
        ]
    ]

validateTests = testGroup "Validation tests"
    [
        testGroup "Duplicates" 
        [
            testCase "dup VAR 1" $
                let vars = [Var "a", Var "a"]
                    ccs = Ccs vars [] [] []
                    (s, w) = execRWS (dupVars ccs) () False
                in case w of
                    [m1] -> return ()
                    _ -> assertFailure "dup 1"
            ,
            testCase "dup VAR 2" $
                let vars = [Var "a", Var "a", Var "a"]
                    ccs = Ccs vars [] [] []
                    (s, w) = execRWS (dupVars ccs) () False
                in case w of
                    [m1] -> return ()
                    tmp -> assertFailure $ show tmp
            ,
            testCase "dup VAR 3" $
                let vars = [Var "b", Var "a", Var "a"]
                    ccs = Ccs vars [] [] []
                    (s, w) = execRWS (dupVars ccs) () False
                in case w of
                    [m1] -> return ()
                    tmp -> assertFailure $ show tmp
            ,
            testCase "dup VAR 4" $
                let vars = [Var "b", Var "a", Var "a", Var "b"]
                    ccs = Ccs vars [] [] []
                    (s, w) = execRWS (dupVars ccs) () False
                in case w of
                    [m1, m2] -> return ()
                    tmp -> assertFailure $ show tmp
            ,
            testCase "dup SIG 1" $
                let sigs = [Sig "add" 2 2, Sig "add" 3 3]
                    ccs = Ccs [] sigs [] []
                    (s,w) = execRWS (dupSigs ccs) () False    
                in if s then return () else assertFailure $ show w
            ,
            testCase "dup Sig 2" $
                let sigs = [Sig "add" 2 2, Sig "add" 3 3, Sig "add" 4 4]
                    ccs = Ccs [] sigs [] []
                    (s,w) = execRWS (dupSigs ccs) () False    
                in 
                    case (s,w) of
                        (True, [m1]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            ,
            testCase "dup Sig 3" $
                let sigs = [Sig "sub" 2 2, Sig "add" 3 3, Sig "add" 4 4]
                    ccs = Ccs [] sigs [] []
                    (s,w) = execRWS (dupSigs ccs) () False    
                in 
                    case (s,w) of
                        (True, [m1]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            ,
            testCase "dup Sig 4" $
                let sigs = [Sig "sub" 2 2, Sig "add" 3 3, Sig "add" 4 4, Sig "sub" 5 5]
                    ccs = Ccs [] sigs [] []
                    (s,w) = execRWS (dupSigs ccs) () False    
                in 
                    case (s,w) of
                        (True, [m1, m2]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            ,
            testCase "overlap 1" $
                let vars = [Var "add"]
                    sigs = [Sig "add" 2 2]
                    ccs = Ccs vars sigs [] []
                    (s, w) = execRWS (overlap ccs) () False
                in
                    case (s,w) of
                        (True, [_]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            ,
            testCase "overlap 2" $
                let vars = [Var "add", Var "sub"]
                    sigs = [Sig "add" 2 2, Sig "sub" 3 4]
                    ccs = Ccs vars sigs [] []
                    (s, w) = execRWS (overlap ccs) () False
                in
                    case (s,w) of
                        (True, [_,_]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            ,
            testCase "overlap 3" $
                let vars = [Var "add", Var "sub"]
                    sigs = [Sig "add" 2 2]
                    sorts = [Sort "sub" Nothing []]
                    ccs = Ccs vars sigs sorts []
                    (s, w) = execRWS (overlap ccs) () False
                in
                    case (s,w) of
                        (True, [_,_]) -> return ()
                        (_, tmp) -> assertFailure $ show tmp
            -- testCase "dup var 1" $
            --     let res = validateCCS (Ccs [Var "a", Var "a"] [] [] [])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()
            -- ,
            -- testCase "dup var 2" $
            --     let res = validateCCS (Ccs [Var "a", Var "a",Var "b", Var "b"] [] [] [])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()                    
            -- ,
            -- testCase "dup sig 1" $
            --     let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2] [] [])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()        
            -- ,
            -- testCase "dup sig 2" $
            --     let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2, Sig "b" 1 1, Sig "b" 2 2] [] [])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()
            -- ,
            -- testCase "dup sig 3" $
            --     let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2, Sig "c" 1 1, Sig "b" 2 2] [] [])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()
            -- ,
            -- testCase "dup sig 4" $
            --     let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "b" 1 1, Sig "c" 2 2] [] [])
            --     in case res of
            --         Nothing -> return ()
            --         Just _ -> assertFailure ""
            -- ,
            -- testCase "undefined var" $
            --     let res = validateCCS (Ccs [Var "a"] [] [] [Rule (Term "add" Nothing) [Term "b" Nothing] Nothing])
            --     in case res of
            --         Nothing -> assertFailure ""
            --         Just _ -> return ()
        ],
        testGroup "Type checking"
        [
            testCase "Lookup env 1" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    res = runTypechecker (lookupEnv "z") sorts
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right Nothing) -> assertFailure $ show res
                    (Right (Just res')) ->
                        res' @?= (head sorts)
            ,
            testCase "Lookup env 2" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    res = runTypechecker (lookupEnv "s") sorts
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right (Just res')) -> assertFailure $ show res
                    (Right Nothing) -> return ()
            ,
            testCase "Term 1" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    term = Term "z" Nothing
                    res = runTypechecker (typecheckTerm "unum" term) sorts
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right []) -> return ()
                    (Right _ ) -> assertFailure $ show res
            ,
            testCase "Term 2" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    term = Term "z" Nothing
                    res = runTypechecker (typecheckTerm "list" term) sorts
                in case res of
                    (Left m1) -> return ()
                    (Right _) -> assertFailure $ show res
            ,
            testCase "Term' 1" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    term = Term "z" Nothing
                    res = runTypechecker (typecheckTerm' term) sorts
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right s) -> return ()
            ,
            testCase "Sub terms 1" $
                let term  = (Term "add" (Just [Term "x" Nothing, Term "y" Nothing]))
                    sort = Sort "add" (Just ["unum", "unum"]) ["unum"]
                    exp = [Sort "x" Nothing ["unum"],Sort "y" Nothing ["unum"]]
                    res = runTypechecker (typecheckTerm "unum" term) [sort]
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right ss) -> ss @?= exp
            ,
            testCase "Conditions 1" $
                let conds = [
                        Cond (Term "add" (Just [Term "z" Nothing, Term "y" Nothing])) 
                             [Term "y" Nothing]
                        ]
                    sorts = [
                        Sort "z" Nothing ["unum"],
                        Sort "add" (Just ["unum", "unum"]) ["unum"]
                        ]
                    res = runTypechecker (typecheckConds conds) sorts
                in case res of
                    (Left _) -> assertFailure $ show res
                    (Right ss) -> ss @?= [Sort "y" Nothing ["unum"]]
        ]

    ]

mosmlTests = testGroup "MosML Tests" 
    [
        testGroup "Preprocessing" 
        [
            testCase "Datatype 1" $
                let sorts = [Sort "z" Nothing ["unum"]]
                    ccs = Ccs [] [] sorts []
                    (res,_) = ccsToTasks ccs
                in res @?= [Datatype "unum" sorts]
            ,
            testCase "Datatype 2" $
                let sorts = [Sort "s" (Just ["unum"]) ["unum"]]
                    ccs = Ccs [] [] sorts []
                    (res,_) = ccsToTasks ccs
                in res @?= [Datatype "unum" sorts]
            ,
            testCase "Datatype 3" $
                let sorts = [Sort "z" Nothing ["unum"], Sort "s" (Just ["unum"]) ["unum"]]
                    ccs = Ccs [] [] sorts []
                    (res,_) = ccsToTasks ccs
                in res @?= [Datatype "unum" sorts]
            ,
            testCase "Datatype 4" $
                let sort1 = Sort "z" Nothing ["Nat"]
                    sort2 = Sort "s" (Just ["unum"]) ["unum"]
                    ccs = Ccs [] [] [sort1, sort2] []
                    (res,_) = ccsToTasks ccs
                in res @?= [Datatype "Nat" [sort1], Datatype "unum" [sort2]]
            ,
            testCase "Function 1" $
                let sigs = [Sig "add" 1 1]
                    rules = [
                        Rule (Term "add" Nothing) [Term "z" Nothing] Nothing
                        ]
                    (res,_) = ccsToTasks $ Ccs [] sigs [] rules
                in res @?= [Function "add" rules]
            ,
            testCase "Function 2" $
                let sigs = [Sig "add" 1 1]
                    rules = [
                        Rule (Term "add" Nothing) [Term "z" Nothing] Nothing,
                        Rule (Term "add" Nothing) [Term "s" (Just [Term "z" Nothing])] Nothing                     
                        ]
                    (res,_) = ccsToTasks $ Ccs [] sigs [] rules
                in res @?= [Function "add" rules]
            ,
            testCase "Function 3" $
                let sigs = [Sig "add" 1 1, Sig "sub" 2 2]
                    rule1 = Rule (Term "add" Nothing) [Term "z" Nothing] Nothing
                    rule2 = Rule (Term "sub" Nothing) [Term "s" (Just [Term "z" Nothing])] Nothing                     
                    (res,_) = ccsToTasks $ Ccs [] sigs [] [rule1, rule2]
                in res @?= [Function "add" [rule1], Function "sub" [rule2]]
        ],
        testGroup "Translation"
        [
            testCase "Sort to string 1" $
                let res = "z"
                    sort = Sort "z" Nothing ["a"]
                    (a,w) = evalRWS (sortToString sort) [] ""
                in res @?= a
            ,
            testCase "Sort to string 2" $
                let res = "s of unum"
                    sort = Sort "s" (Just ["unum"]) ["it"]
                    (a,w) = evalRWS (sortToString sort) [] ""
                in res @?= a
            ,
            testCase "Sort to string 3" $
                let res = "s of unum * t2"
                    sort = Sort "s" (Just ["unum", "t2"]) ["it"]
                    (a,w) = evalRWS (sortToString sort) [] ""
                in res @?= a
            , 
            testCase "Datatype to string 1" $
                let res = "datatype add =\n\tz\n\t|s of unum"
                    task = Datatype "add" [Sort "z" Nothing ["add"], Sort "s" (Just ["unum"]) ["add"]]
                    (a,w) = evalRWS (taskToString task) [] ""
                in a @?= res
            ,
            testCase "Term to string 1" $
                let exp = "add x y"
                    term = Term "add" (Just [Term "x" Nothing, Term "y" Nothing])
                    (a,w) = evalRWS (termToString term) [] ""
                in a @?= exp
            ,
            testCase "Term to string 2" $
                let exp = "add (s x) y"
                    term = Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing])
                    (a,w) = evalRWS (termToString term) [] ""
                in a @?= exp
            ,
            testCase "Cond to string 1" $
                let exp = "val z = add x y"
                    cond = Cond 
                        (Term "add" (Just [Term "x" Nothing, Term "y" Nothing]))
                        [Term "z" Nothing]
                    (a,w) = evalRWS (condTostring cond) [] ""
                in a @?= exp
            ,
            testCase "Cond to string 2" $
                let exp = "val s z = add (s x) y"
                    cond = Cond 
                        (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                        [Term "s" (Just [Term "z" Nothing])]
                    (a,w) = evalRWS (condTostring cond) [] ""
                in a @?= exp
            ,
            testCase "Cond to string 3" $
                let exp = "val s (s z) = add (s x) y"
                    cond = Cond 
                        (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                        [Term "s" (Just [Term "s" (Just [Term "z" Nothing])])]
                    (a,w) = evalRWS (condTostring cond) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 1" $
                let exp = "add z y = y"
                    rule = Rule
                            (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                            [Term "y" Nothing]
                            Nothing
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 2" $
                let exp = "add z y = s y"
                    rule = Rule
                            (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                            [Term "s" (Just [Term "y" Nothing])]
                            Nothing
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 3" $
                let exp = "add z y = s (s y)"
                    rule = Rule
                            (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                            [Term "s" (Just [Term "s" (Just [Term "y" Nothing])])]
                            Nothing
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 4" $
                let exp = "add (s x) y = s (s y)"
                    rule = Rule
                            (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                            [Term "s" (Just [Term "s" (Just [Term "y" Nothing])])]
                            Nothing
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 5" $
                let exp = "add (s x) y = let val s a = add x y in s z end"
                    rule = Rule
                            (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                            [Term "s" (Just [Term "z" Nothing])]
                            (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) [Term "s" (Just [Term "a" Nothing])]])
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Rule to string 6" $
                let exp = "add (s x) y = let val s a = add x y\n\t val it = a in s z end"
                    rule = Rule
                            (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                            [Term "s" (Just [Term "z" Nothing])]
                            (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) [Term "s" (Just [Term "a" Nothing])],
                                   Cond (Term "a" Nothing) [Term "it" Nothing]])
                    (a,w) = evalRWS (ruleToString rule) [] ""
                in a @?= exp
            ,
            testCase "Function to string 1" $
                let exp = "fun  add z y = y"
                    fun = Function "add" [
                            Rule 
                                (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                                [Term "y" Nothing]
                                Nothing
                            ]
                    (a,_) = evalRWS (taskToString fun) [] "fun  "
                in a @?= exp
            ,
            testCase "Function to string 2" $
                let exp = "fun  add (s x) y = let val z = add x y in s a end"
                    fun = Function "add" [
                            Rule 
                                (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                                [Term "s" (Just [Term "a" Nothing])]
                                (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) 
                                       [Term "z" Nothing]
                                    ])
                            ]
                    (a,_) = evalRWS (taskToString fun) [] "fun  "
                in a @?= exp
            ,
            testCase "Function to string 3" $
                let exp = "fun  add z y = y\n\t|add (s x) y = let val z = add x y in s a end"
                    fun = Function "add" [
                            Rule 
                                (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                                [Term "y" Nothing]
                                Nothing
                            ,
                            Rule 
                                (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                                [Term "s" (Just [Term "a" Nothing])]
                                (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) 
                                       [Term "z" Nothing]
                                    ])
                            ]
                    (a,_) = evalRWS (taskToString fun) [] "fun  "
                in a @?= exp
            ,
            testCase "Multiple Tasks 1" $
                let exp = "datatype unum =\n\tz\n\t|s of unum\n\nfun  add z y = y\n\t|add (s x) y = let val z = add x y in s a end"
                    fun = Function "add" [
                            Rule 
                                (Term "add" (Just [Term "z" Nothing, Term "y" Nothing]))
                                [Term "y" Nothing]
                                Nothing
                            ,
                            Rule 
                                (Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing]))
                                [Term "s" (Just [Term "a" Nothing])]
                                (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) 
                                       [Term "z" Nothing]
                                    ])
                            ]
                    dat = Datatype "unum" [
                                Sort "z" Nothing ["unum"],
                                Sort "s" (Just ["unum"]) ["unum"]
                            ]
                    (a,_) = evalRWS (tasksToString [dat, fun]) [] "fun  "
                in a @?= exp
            -- ,
            -- testCase "multiple return" $

        ]
    ]


-- Combine all the test groups and run 
allTests = testGroup "All Tests" [parserTest, validateTests, mosmlTests]
main = defaultMain allTests