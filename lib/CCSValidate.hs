

module CCSValidate where


import Data.List

import CCSAst

-- entry point validation function. Will return nothing in case of success
-- and a list of error messages in case of errors found
-- valdidate :: CCS -> Maybe [ErrMsg]
-- validate (Ccs (Var vars) (Sig sigs) (Sort sorts) (Rules rules)) = 
    



-- validateDuplicate :: [Id] -> String -> [ErrMsg]
-- validateDuplicate ids msg = 
--     let nubs = nub ids in
--     idToErrMsg nubs msg

-- Converts a list of ids to a list of error messages
idToErrMsg :: [Id] -> String -> [ErrMsg]
idToErrMsg [] _ = []
idToErrMsg (id:ids) msg =
    let msg'  = [msg ++ id ++ "\n"]
        msgs = idToErrMsg ids msg
    in msg' ++ msgs