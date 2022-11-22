

module CCSMosml where

import CCSAst
import Data.List

{-
This module will preprocess the AST and produce a list of Tasks that will then
be translated to the MosML source code.

Datatype values will be converted to MosML dataatypes and Functions to MosML functions
-}


data Task =
    Datatype Id [Constructor]
    | Function Id [Rule]
    deriving (Eq, Read, Show)
{-
Defines a constructor of a datatype in MosML. The Id is the name of the 
constructor and the maybe id list is the constructor arguments. We need a
new datatype for this, as the only thing we have from the CCS AST is the sorts
and now we need to create some structure from them. 

datatype Nat
    | z           <-- Constructor   (Nothing)
    | s of Nat    <-- Constructor   (Just)
-}
data Constructor = Constructor Id (Maybe [Id])
    deriving (Eq, Read, Show)



toDatatype :: CCS -> [Task]
toDatatype (Ccs _ sigs sorts _) =
    -- find all constructor sorts
    let sorts' =
            let sigIds   = getSigIds sigs
                sortIds  = getSortIds sorts
                sortIds' = filter (`notElem` sigIds) sortIds
            in filter (\x-> let (Sort id _ _) = x in id `elem` sortIds') sorts

    -- find unique return sorts/types (datatype ids)
        dtIds = nub $ getReturnSort sorts'

    -- create the actual datatypes
    in toDatatype' dtIds sorts'

-- expects id list to contain unique ids
-- TODO: What happens if sorts is longer than ids?
toDatatype' :: [Id] -> [Sort] -> [Task]
toDatatype' [] _ = []
toDatatype' (id:ids) sorts =
    let (sorts', sorts'') = partition (\(Sort _ _ [ret]) -> ret == id) sorts
        const = map (\(Sort sid args _) -> Constructor sid args) sorts'
    in Datatype id const : toDatatype' ids sorts''
        


getReturnSort :: [Sort] -> [Id]
getReturnSort [] = []
getReturnSort (s:ss) = 
    let (Sort _ _ [ret]) = s in
    ret : getReturnSort ss

getSortIds :: [Sort] -> [Id]
getSortIds [] = []
getSortIds (s:ss) =
    let (Sort id _ _) = s in
    id : getSortIds ss

getSigIds :: [Sig] -> [Id]
getSigIds [] = []
getSigIds (s:ss) =
    let (Sig id _ _) = s in
    id : getSigIds ss








