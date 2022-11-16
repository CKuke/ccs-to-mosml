module CCSParser where

import CCSAst

import Text.Parsec
import Text.Parsec.Char


-- Parser with stream type string, user state () and return type a
type Parser a = Parsec String () a 

-- Parser entry point
parseProgram :: String -> Either ErrMsg CCS
parseProgram str =
    case runParser parseCCS () "" str of
        Right ccs -> Right ccs
        Left err -> Left (show err)


-- 
-- Utility parsers
--

whitespace :: Parser ()
whitespace = do
    spaces
    return ()

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

parseIdList :: Parser [Id]
parseIdList = lexeme $ do
    sepEndBy parseId (many1 space)

parseId :: Parser Id
parseId = do
    id <- many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z']
    return id


parseFunList :: Parser [(Id, Int, Int)]
parseFunList = 
    sepEndBy parseFun (many1 space)

parseFun :: Parser (Id, Int, Int)
parseFun = do
    char '('
    whitespace
    id <- parseId
    whitespace
    arity <- many1 digit
    whitespace
    coarity <- many1 digit
    whitespace
    char ')'
    return (id, read arity, read coarity)

parseSortList :: Parser [(Id, [Id], [Id])]
parseSortList = lexeme $ do
    sepEndBy parseSortDef (many1 space)

parseSortDef :: Parser (Id, [Id], [Id])
parseSortDef = do
    char '('
    whitespace
    id <- parseId
    whitespace
    ins <- parseIdList <|> return []
    whitespace
    string "->"
    whitespace
    outs <- parseIdList
    whitespace
    char ')'
    return (id, ins, outs)

parseRuleList :: Parser [Rule]
parseRuleList = lexeme $ do
    sepBy parseRuleDef spaces

parseRuleDef :: Parser Rule
parseRuleDef = lexeme $ do
    left <- parseTerm
    whitespace
    string "->"
    whitespace
    char '<'
    whitespace
    right <- parseTermList
    whitespace
    char '>'
    whitespace
    conds <- parseConds
    return $ Rule left right conds

parseConds :: Parser [Cond]
parseConds = 
    option [] $ do
        string "<="
        whitespace
        conds <- parseCondList
        whitespace
        return conds

-- lexeme $ do
--     choice [
--         do  string "<="
--             whitespace
--             conds <- parseCondList
--             whitespace
--             return $ conds,
--         do  return []
--         ]

-- lexeme $ do
--     try (do
--         string "<="
--         whitespace
--         conds <- parseCondList
--         whitespace
--         return $ conds
--         )
--     <|> do 
--     return []


parseCondList :: Parser [Cond]
parseCondList = lexeme $ do
    sepBy1 parseCond (whitespace >> char '^' >> whitespace)

parseCond :: Parser Cond
parseCond = do
    term <- parseTerm
    whitespace
    string "->"
    whitespace
    char '<'
    whitespace
    terms <- parseTermList
    whitespace
    char '>'
    whitespace
    return $ Cond term terms

parseTermList :: Parser [Term]
parseTermList = lexeme $ do
    sepBy parseTerm (whitespace >> char ',' >> whitespace) 

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


-- The main entry point for parsing
parseCCS :: Parser CCS
parseCCS = -- return $ Ccs (Var []) (Sig []) (Sort []) (Rules [])
    lexeme $ do
    var <- parseVar
    sig <- parseSig
    sort <- parseSort
    rules <- parseRules
    return $ Ccs var sig sort rules

-- Parse the VAR section of the program
parseVar :: Parser VAR
parseVar = lexeme $ do
    char '('
    whitespace
    string "VAR"
    whitespace
    ids <- parseIdList
    whitespace
    char ')'
    return $ Var ids


-- Parse the SIG section of the program
parseSig :: Parser SIG
parseSig = lexeme $ do
    char '('
    whitespace
    string "SIG"
    whitespace
    funs <- parseFunList
    whitespace
    char ')'
    return $ Sig funs

-- Parse the SORT section of the program
parseSort :: Parser SORT
parseSort = lexeme $ do
    char '('
    whitespace
    string "SORT"
    whitespace
    sorts <- parseSortList
    whitespace
    char ')'
    return $ Sort sorts


parseRules :: Parser RULES
parseRules = lexeme $ do
    char '('
    whitespace
    string "RULES"
    whitespace
    rules <- parseRuleList
    whitespace
    char ')'
    return $ Rules rules


