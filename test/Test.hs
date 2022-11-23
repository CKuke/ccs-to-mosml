{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.Error

import CCSAst
import CCSParser
import CCSValidate
import CCSMosml 
import CCSMosml (constructorsToStrings)

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



            -- testCase "add example rule 2" $
            --     runParser parseRuleList () "" "add(zero, y) -> <y>\nadd(succ(x), y) -> <succ(z)> <= add(x,y) -> <z>\n"
            --     @?= Right [
            --         Rule 
            --         (Term "add" [Term "zero" [], Term "y" []])
            --         [Term "y" []]
            --         [] -- no conds
            --         ,
            --         Rule
            --         (Term "add" [Term "succ" [Term "x" []], Term "y" []])
            --         [Term "succ" [Term "z" []]]
            --         [Cond (Term "add" [Term "x" [], Term "y" []]) [Term "z" []]]
            --         ],
            -- testCase "add example rule 2 (RULES)" $
            --     runParser parseRules () "" "(RULES \nadd(zero, y) -> <y>\nadd(succ(x), y) -> <succ(z)> <= add(x,y) -> <z> \n )"
            --     @?= Right ( Rules [
            --         Rule 
            --         (Term "add" [Term "zero" [], Term "y" []])
            --         [Term "y" []]
            --         [] -- no conds
            --         ,
            --         Rule
            --         (Term "add" [Term "succ" [Term "x" []], Term "y" []])
            --         [Term "succ" [Term "z" []]]
            --         [Cond (Term "add" [Term "x" [], Term "y" []]) [Term "z" []]]
            --         ])
        ],
        -- testGroup "COMMENT" 
        -- [

        -- ],
        testGroup "CCS"
        [
            testCase "add" $ do
                tmp <- (parseProgram <$> readFile "examples/add.ccs")
                let var = [Var "x", Var "y", Var "z"]
                    sig = [Sig "add" 2 1]
                    sort = [
                        Sort "zero" Nothing ["Nat"],
                        Sort "succ" (Just ["Nat"]) ["Nat"],
                        Sort "add" (Just ["Nat", "Nat"]) ["Nat"]
                        ]
                    rules = [
                        Rule
                            (Term "add" (Just [Term "zero" Nothing, Term "y" Nothing]))
                            ([Term "y" Nothing])
                            Nothing
                        ,Rule
                            (Term "add" (Just [Term "succ"(Just [Term "x" Nothing]), Term "y" Nothing]))
                            ([Term "succ" (Just [Term "z" Nothing])])
                            (Just [Cond (Term "add" (Just [Term "x" Nothing, Term "y" Nothing])) [Term "z" Nothing]])
                        ]
                    in tmp @?= Right (Ccs var sig sort rules)
        ]
    ]

validateTests = testGroup "Validation tests"
    [
        testGroup "Duplicates" 
        [
            testCase "dup var 1" $
                let res = validateCCS (Ccs [Var "a", Var "a"] [] [] [])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()
            ,
            testCase "dup var 2" $
                let res = validateCCS (Ccs [Var "a", Var "a",Var "b", Var "b"] [] [] [])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()                    
            ,
            testCase "dup sig 1" $
                let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2] [] [])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()        
            ,
            testCase "dup sig 2" $
                let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2, Sig "b" 1 1, Sig "b" 2 2] [] [])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()
            ,
            testCase "dup sig 3" $
                let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "a" 2 2, Sig "c" 1 1, Sig "b" 2 2] [] [])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()
            ,
            testCase "dup sig 4" $
                let res = validateCCS (Ccs [] [Sig "a" 1 1, Sig "b" 1 1, Sig "c" 2 2] [] [])
                in case res of
                    Nothing -> return ()
                    Just _ -> assertFailure ""
            ,
            testCase "undefined var" $
                let res = validateCCS (Ccs [Var "a"] [] [] [Rule (Term "add" Nothing) [Term "b" Nothing] Nothing])
                in case res of
                    Nothing -> assertFailure ""
                    Just _ -> return ()
        ]
    ]

mosmlTests = testGroup "MosML Tests" 
    [
        testGroup "Preprocessing" 
        [
            testCase "toDatatype' 1" $
                let ids = ["unum"]
                    sorts = [Sort "C" Nothing ["unum"]]
                    res = toDatatype' ids sorts
                in res @?= [Datatype "unum" [Constructor "C" Nothing]]
            ,
            testCase "toDatatype' 2" $
                let ids = ["unum"]
                    sorts = [Sort "z" Nothing ["unum"], Sort "s" (Just ["unum"]) ["unum"]]
                    res = toDatatype' ids sorts
                in res @?= [Datatype "unum" [Constructor "z" Nothing, Constructor "s" (Just ["unum"])]]
            ,
            testCase "toDatatype' 3" $
                let ids = ["unum", "list"]
                    sorts = [Sort "z" Nothing ["unum"], Sort "s" (Just ["unum"]) ["unum"], Sort "nil" Nothing ["list"]]
                    res = toDatatype' ids sorts
                in res @?= [Datatype "unum" [Constructor "z" Nothing, Constructor "s" (Just ["unum"])],
                            Datatype "list" [Constructor "nil" Nothing]]
            ,
            testCase "tofunction' 1" $
                let ids = ["add"]
                    rules = [Rule (Term "add" Nothing) [Term "x" Nothing] Nothing]
                    res = toFunction' ids rules
                in res @?= [Function "add" rules]
            ,
            testCase "tofunction' 2" $
                let ids = ["add"]
                    rules = [Rule (Term "add" Nothing) [Term "x" Nothing] Nothing,
                            Rule (Term "add" Nothing) [Term "y" Nothing] Nothing]
                    res = toFunction' ids rules
                in res @?= [Function "add" rules]
            ,
            testCase "tofunction' 3" $
                let ids = ["add", "sub"]
                    rules1 = [Rule (Term "add" Nothing) [Term "x" Nothing] Nothing,
                            Rule (Term "add" Nothing) [Term "y" Nothing] Nothing]
                    rules2 = [Rule (Term "sub" Nothing) [Term "x" Nothing] Nothing]
                    res = toFunction' ids (rules1 ++ rules2)
                in res @?= [Function "add" rules1, Function "sub" rules2]
        ],
        testGroup "Translation"
        [
            testCase "constructors To Strings 1" $
                let cons = [Constructor "z" Nothing]
                    res = constructorsToStrings cons
                in res @?= ["z"]
            ,
            testCase "constructors To Strings 2" $
                let cons = [Constructor "z" Nothing, Constructor "s" (Just ["x", "y", "z"])]
                    res = constructorsToStrings cons
                in res @?= ["z", "s of x * y * z"]
            ,
            testCase "task to string - datatype 1" $ 
                let tasks = [Datatype "unum" ([Constructor "z" Nothing])]
                    res = taskToString tasks
                in res @?= "datatype unum =\n\tz"
            ,
            testCase "task to string - datatype 2" $ 
                let tasks = [Datatype "unum" ([Constructor "z" Nothing, Constructor "s" (Just ["unum"])])]
                    res = taskToString tasks
                in res @?= "datatype unum =\n\tz\n\t| s of unum"
            ,
            testCase "term to string 1" $
                let term = Term "add" Nothing
                    res = termToString term
                in res @?= "add"
            ,
            testCase "term to string 2" $
                let term = Term "add" (Just [Term "x" Nothing, Term "y" Nothing])
                    res = termToString term
                in res @?= "add x y"
            ,
            testCase "term to string 3" $
                let term = Term "add" (Just [Term "s" (Just [Term "x" Nothing]), Term "y" Nothing])
                    res = termToString term
                in res @?= "add (s x) y"
            ,
            testCase "term to string 4" $
                let term = Term "add" (Just [Term "s" (Just [Term "s" (Just [Term "x" Nothing])]), Term "y" Nothing])
                    res = termToString term
                in res @?= "add (s (s x)) y"
            -- TODO: Create tests for condsToStrings
        ]
    ]


-- Combine all the test groups and run 
allTests = testGroup "All Tests" [parserTest, validateTests, mosmlTests]
main = defaultMain allTests