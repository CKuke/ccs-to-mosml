
-- Grammar:

-- ccs      ::= (VAR idlist) (SIG funlist) (SORT sortlist) (RULES rulelist) [(COMMENT string)]
-- idlist   ::= ε | id idlist

-- funlist  ::= ε | fun funlist
-- fun      ::= (id int int)

-- sortlist ::= ε | sort sortlist
-- sort     ::= (id idlist -> idlist)

-- rulelist ::= ε | rule rulelist
-- rule     ::= term -> < termlist > conds

-- conds    ::= ε | <= condlist
-- condlist ::= cond | cond ^ condlist
-- cond     ::= term -> < termlist >

-- termlist ::= term | term, termlist
-- term     ::= id | id(termlist)




module CCSAst where

type ErrMsg = String
type Id = String

-- The output after parsing
-- data CCS = Ccs {idlist :: VAR, funlist :: SIG, sortlist :: SORT, rulelist :: RULES}
data CCS = Ccs VAR SIG SORT RULES
    deriving (Eq, Read, Show)

newtype VAR = Var [Id]
    deriving (Eq, Read, Show)

newtype SIG = Sig [(Id, Int, Int)]
    deriving (Eq, Read, Show)

newtype SORT = Sort [(Id, [Id], [Id])]
    deriving (Eq, Read, Show)

newtype RULES = Rules [Rule]
    deriving (Eq, Read, Show)




data Term = Term Id [Term] -- List might be empty
    deriving (Eq, Read, Show)

data Rule = Rule Term [Term] [Cond] -- Cond list might be empty
    deriving (Eq, Read, Show)

data Cond = Cond Term [Term]
    deriving (Eq, Read, Show)






