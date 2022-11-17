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
    var <- parseVAR
    sig <- parseSIG
    sort <- parseSORT
    rules <- parseRULES
    return $ Ccs var sig sort rules

-- Parse the VAR section of the program
parseVAR :: Parser [Var]
parseVAR = lexeme $ do
    symbol "("
    symbol  "VAR"
    ids <- parseIdList
    symbol ")"
    return $ map Var ids


-- Parse the SIG section of the program
parseSIG :: Parser [Sig]
parseSIG = lexeme $ do
    symbol "("
    symbol "SIG"
    sigs <- parseSigList
    symbol ")"
    return sigs

-- Parse the SORT section of the program
parseSORT :: Parser [Sort]
parseSORT = lexeme $ do
    symbol "("
    symbol "SORT"
    sorts <- parseSortList
    symbol ")"
    return sorts

-- Parse the RULES section of the program
parseRULES :: Parser [Rule]
parseRULES = lexeme $ do
    symbol "("
    symbol "RULES"
    rules <- parseRuleList
    symbol ")"
    return rules


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
    let chars = ['a'..'z'] ++ ['A'..'Z']
    c <- oneOf chars
    cs <- many $ oneOf (chars ++ ['0'..'9'])
    return (c:cs)

-- funlist  ::= ε | fun funlist
parseSigList :: Parser [Sig]
parseSigList = lexeme $ do many parseSig

-- fun      ::= (id int int)
parseSig :: Parser Sig
parseSig = lexeme $ do
    symbol "("
    id      <- parseId
    arity   <- lexeme $ many1 digit
    coarity <- lexeme $ many1 digit
    symbol ")"
    return $ Sig id (read arity) (read coarity)

-- sortlist ::= ε | sort sortlist
parseSortList :: Parser [Sort]
parseSortList = lexeme $ do many parseSortDef

-- sort     ::= (id idlist -> idlist)
parseSortDef :: Parser Sort
parseSortDef = lexeme $ do
    symbol "("
    id  <- parseId
    ins <- parseIdList 
    symbol "->"
    outs <- parseIdList
    symbol ")"
    case ins of
        [] -> return $ Sort id Nothing outs
        ins' -> return $ Sort id (Just ins') outs

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
parseConds :: Parser (Maybe [Cond])
parseConds =
    option Nothing $ do
        symbol "<="
        conds <- parseCondList
        return $ Just conds

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
            return $ Term id (Just terms)
        )
    <|> do  id <- parseId
            return $ Term id Nothing





