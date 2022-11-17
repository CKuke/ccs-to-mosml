
-- Grammar:

-- ccs      ::= (VAR idlist) (SIG siglist) (SORT sortlist) (RULES rulelist) [(COMMENT string)]
-- idlist   ::= ε | id idlist

-- siglist  ::= ε | sig siglist
-- sig      ::= (id int int)

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

newtype Var  = Var Id
    deriving (Eq, Read, Show)

data Sig  = Sig Id Int Int
    deriving (Eq, Read, Show)

data Sort = Sort Id (Maybe [Id]) [Id] 
    deriving (Eq, Read, Show)

data Rule = Rule Term [Term] (Maybe [Cond])
    deriving (Eq, Read, Show)

data Term = Term Id (Maybe [Term])
    deriving (Eq, Read, Show)

data Cond = Cond Term [Term]
    deriving (Eq, Read, Show)

-- The output after parsing
-- data CCS = Ccs {idlist :: VAR, funlist :: SIG, sortlist :: SORT, rulelist :: RULES}
data CCS = Ccs [Var] [Sig] [Sort] [Rule]
    deriving (Eq, Read, Show)












------------------------------------------------------

-- data TransTask = 
--     Datatype [Sort]
--     | Function [Rule]


