module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.Error

import CCSAst
import CCSParser

parserTest = testGroup "Parser tests"
    [
        testGroup "VAR"
        [
            testCase "empty 1" $
                runParser parseVar () "" "(VAR)" 
                @?= Right (Var []),
            testCase "empty 2" $
                runParser parseVar () "" "(VAR )" 
                @?= Right (Var []),
            testCase "var 1" $
                runParser parseVar () "" "(VAR a )" 
                @?= Right (Var ["a"]),
            testCase "var 2" $
                runParser parseVar () "" "(VAR a b)" 
                @?= Right (Var ["a", "b"]),
            testCase "fail 1" $
                case runParser parseVar () "" "(VAR a" of
                    Right (Var _) -> assertFailure "fail"
                    _ -> return ()   
        ],
        testGroup "SIG" 
        [
            testCase "single fun" $
                runParser parseFun () "" "(add 2 1)"
                @?= Right ("add", 2, 1),
            testCase "multiple fun" $
                runParser parseSig () "" "(SIG (add 2 1)\n(sub 42 90) )"
                @?= Right (Sig [("add", 2, 1), ("sub", 42, 90)])
        ],
        testGroup "SORT" 
        [
            testCase "single sort" $
                runParser parseSort () "" "(SORT (succ Nat -> Nat))"
                @?= Right (Sort [("succ", ["Nat"], ["Nat"])]),
            testCase "base case sort" $
                runParser parseSort () "" "(SORT (nil -> List))"
                @?= Right (Sort [("nil", [], ["List"])]),
            testCase "unary" $
                runParser parseSort () "" "(SORT (z -> Nat)\n(succ Nat -> Nat))"
                @?= Right (Sort [("z", [], ["Nat"]), ("succ", ["Nat"], ["Nat"])])
        ],
        testGroup "RULE" 
        [
            testCase "term 1" $
                runParser parseTerm () "" "ident"
                @?= Right (Term "ident" []),
            testCase "term 2" $
                runParser parseTerm () "" "ident(a, b)"
                @?= Right (Term "ident" [Term "a" [], Term "b" []]),
            testCase "termlist 1" $
                runParser parseTermList () "" "a"
                @?= Right ([Term "a" []]),
            testCase "termlist 2" $
                runParser parseTermList () "" "a, b, c"
                @?= Right ([Term "a" [], Term "b" [], Term "c" []]),
            testCase "single cond" $
                runParser parseCond () "" "add(a, b) -> <c>"
                @?= Right (Cond 
                    (Term "add" [Term "a" [], Term "b" []]) 
                    [Term "c" []]),
            testCase "single cond more output" $
                runParser parseCond () "" "add(a, b) -> <x, s(y)>"
                @?= Right (Cond 
                    (Term "add" [Term "a" [], Term "b" []]) 
                    [Term "x" [], Term "s" [Term "y" []]]),
            testCase "cond conjunction" $
                runParser parseCondList () "" "add(a,b) -> <c> ^ sub(x, y) -> <z>"
                @?= Right ([
                    Cond (Term "add" [Term "a" [], Term "b" []]) [Term "c" []],
                    Cond (Term "sub" [Term "x" [], Term "y" []]) [Term "z" []]
                    ]),
            testCase "conds non empty" $
                runParser parseConds () "" "<= add(a, b) -> <c>"
                @?= Right [
                    Cond (Term "add" [Term "a" [], Term "b" []]) [Term "c" []]
                    ],
            testCase "conds empty" $
                runParser parseConds () "" "empty"
                @?= Right [],
            testCase "single rule" $
                runParser parseRuleDef () "" "l -> <r>"
                @?= Right (Rule (Term "l" []) [Term "r" []] []),
            testCase "single rule with cond" $
                runParser parseRuleDef () "" "l -> <r> <= a -> <b>"
                @?= Right (Rule (Term "l" []) [Term "r" []] [Cond (Term "a" []) [Term "b" []]]),
            testCase "two rules" $
                runParser parseRuleList () "" "a -> <b>\n x -> <y>"
                @?= Right ([
                    Rule (Term "a" []) [Term "b" []] [],
                    Rule (Term "x" []) [Term "y" []] []
                    ]),
            testCase "two rules whith RULES" $
                runParser parseRules () "" "(RULES a -> <b>\n x -> <y>)"
                @?= Right (Rules [
                    Rule (Term "a" []) [Term "b" []] [],
                    Rule (Term "x" []) [Term "y" []] []
                    ]),
            testCase "add example rule" $
                runParser parseRuleDef () "" "add(succ(x), y) -> <succ(z)> <= add(x,y) -> <z>"
                @?= Right (Rule 
                    (Term "add" [Term "succ" [Term "x" []], Term "y" []])
                    [Term "succ" [Term "z" []]]
                    [Cond (Term "add" [Term "x" [], Term "y" []]) [Term "z" []]]
                    ),
            testCase "add example rule 2" $
                runParser parseRuleList () "" "add(zero, y) -> <y>\nadd(succ(x), y) -> <succ(z)> <= add(x,y) -> <z>\n"
                @?= Right [
                    Rule 
                    (Term "add" [Term "zero" [], Term "y" []])
                    [Term "y" []]
                    [] -- no conds
                    ,
                    Rule
                    (Term "add" [Term "succ" [Term "x" []], Term "y" []])
                    [Term "succ" [Term "z" []]]
                    [Cond (Term "add" [Term "x" [], Term "y" []]) [Term "z" []]]
                    ],
            testCase "add example rule 2 (RULES)" $
                runParser parseRules () "" "(RULES \nadd(zero, y) -> <y>\nadd(succ(x), y) -> <succ(z)> <= add(x,y) -> <z> \n )"
                @?= Right ( Rules [
                    Rule 
                    (Term "add" [Term "zero" [], Term "y" []])
                    [Term "y" []]
                    [] -- no conds
                    ,
                    Rule
                    (Term "add" [Term "succ" [Term "x" []], Term "y" []])
                    [Term "succ" [Term "z" []]]
                    [Cond (Term "add" [Term "x" [], Term "y" []]) [Term "z" []]]
                    ])
        ],
        testGroup "COMMENT" 
        [

        ],
        testGroup "CCS"
        [
            testCase "add" $ do
                tmp <- (fmap parseProgram $ readFile "examples/add.ccs")
                tmp @?= Right (Ccs 
                    (Var ["x","y","z"])
                    (Sig [("add",2,1)]) 
                    (Sort [
                        ("zero",[],["Nat"]),
                        ("succ",["Nat"],["Nat"]),
                        ("add",["Nat"],["Nat","Nat"])
                        ]
                    ) 
                    (Rules [
                        Rule (Term "add" [Term "zero" [],Term "y" []]) [Term "y" []] [],
                        Rule (Term "add" [Term "succ" [Term "x" []],Term "y" []]) [Term "succ" [Term "z" []]] [Cond (Term "add" [Term "x" [],Term "y" []]) [Term "z" []]]
                        ]
                    )
                    )
                -- Right (
                --     Ccs 
                --         (Var []) 
                --         (Sig []) 
                --         (Sort []) 
                --         (Rules [])
                --     )
        ]
    ]


-- Combine all the test groups and run 
allTests = testGroup "All Tests" [parserTest]
main = defaultMain allTests