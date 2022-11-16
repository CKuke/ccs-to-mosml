{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module CCSParser where

import CCSAst

import Text.Parsec
import Text.Parsec.Char


-- Parser with stream type string, user state () and return type a
type Parser a = Parsec String () a


{------------------------------------------------------------------------------
    Parser Entry point
-------------------------------------------------------------------------------}
parseProgram :: String -> Either ErrMsg CCS
parseProgram str =
    case runParser parseCCS () "" str of
        Right ccs -> Right ccs
        Left err -> Left (show err)


{------------------------------------------------------------------------------
    Utility parsers
-------------------------------------------------------------------------------}

-- Whille remove all kinds of whitespace
whitespace :: Parser ()
whitespace = do
    spaces
    return ()

-- Remove all whitespace before applying the parser p
lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

-- Parses a symbol such as "VAR", "<", "->" and so on. Consumes prededing white space
symbol :: String -> Parser ()
symbol sym = lexeme $ do
    string sym
    return ()


{------------------------------------------------------------------------------
    Parsers for each of the differen sets VAR, SIG, SORT and RULES
    plus the one putting it all together
------------------------------------------------------------------------------}

parseCCS :: Parser CCS
parseCCS = lexeme $ do
    var <- parseVar
    sig <- parseSig
    sort <- parseSort
    rules <- parseRules
    return $ Ccs var sig sort rules

-- Parse the VAR section of the program
parseVar :: Parser VAR
parseVar = lexeme $ do
    symbol "("
    symbol  "VAR"
    ids <- parseIdList
    symbol ")"
    return $ Var ids


-- Parse the SIG section of the program
parseSig :: Parser SIG
parseSig = lexeme $ do
    symbol "("
    symbol "SIG"
    funs <- parseFunList
    symbol ")"
    return $ Sig funs

-- Parse the SORT section of the program
parseSort :: Parser SORT
parseSort = lexeme $ do
    symbol "("
    symbol "SORT"
    sorts <- parseSortList
    symbol ")"
    return $ Sort sorts

-- Parse the RULES section of the program
parseRules :: Parser RULES
parseRules = lexeme $ do
    symbol "("
    symbol "RULES"
    rules <- parseRuleList
    symbol ")"
    return $ Rules rules


{------------------------------------------------------------------------------
    Parsers for different productions in the grammar. For complete grammar see 
    CCSAst.hs.
------------------------------------------------------------------------------}


-- idlist ::=  ε | id idlist
parseIdList :: Parser [Id]
parseIdList = lexeme $ do many parseId

-- id is a string consisting of lower and upper case letters of the
-- english alphabet
parseId :: Parser Id
parseId = lexeme $ do
    many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z']

-- funlist  ::= ε | fun funlist
parseFunList :: Parser [(Id, Int, Int)]
parseFunList = lexeme $ do many parseFun

-- fun      ::= (id int int)
parseFun :: Parser (Id, Int, Int)
parseFun = lexeme $ do
    symbol "("
    id      <- parseId
    arity   <- lexeme $ many1 digit
    coarity <- lexeme $ many1 digit
    symbol ")"
    return (id, read arity, read coarity)

-- sortlist ::= ε | sort sortlist
parseSortList :: Parser [(Id, [Id], [Id])]
parseSortList = lexeme $ do many parseSortDef

-- sort     ::= (id idlist -> idlist)
parseSortDef :: Parser (Id, [Id], [Id])
parseSortDef = lexeme $ do
    symbol "("
    id  <- parseId
    ins <- parseIdList <|> return []
    symbol "->"
    outs <- parseIdList
    symbol ")"
    return (id, ins, outs)

-- rulelist ::= ε | rule rulelist
parseRuleList :: Parser [Rule]
parseRuleList = lexeme $ do many parseRuleDef

-- rule     ::= term -> < termlist > conds
parseRuleDef :: Parser Rule
parseRuleDef = lexeme $ do
    left <- parseTerm
    symbol "->"
    symbol "<"
    right <- parseTermList
    symbol ">"
    conds <- parseConds
    return $ Rule left right conds

-- conds    ::= ε | <= condlist
parseConds :: Parser [Cond]
parseConds =
    option [] $ do
        symbol "<="
        parseCondList

-- condlist ::= cond | cond ^ condlist
parseCondList :: Parser [Cond]
parseCondList = lexeme $ do
    sepBy1 parseCond $ symbol "^"

-- cond     ::= term -> < termlist >
parseCond :: Parser Cond
parseCond = lexeme $ do
    term <- parseTerm
    symbol "->"
    symbol "<"
    terms <- parseTermList
    symbol ">"
    return $ Cond term terms

-- termlist ::= term | term, termlist
parseTermList :: Parser [Term]
parseTermList = lexeme $ do
    sepBy parseTerm $ symbol ","

-- term     ::= id | id(termlist)
parseTerm :: Parser Term
parseTerm = lexeme $ do
    try (do
            id <- parseId
            whitespace
            char '('
            whitespace
            terms <- parseTermList
            whitespace
            char ')'
            return $ Term id terms
        )
    <|> do  id <- parseId
            return $ Term id []





