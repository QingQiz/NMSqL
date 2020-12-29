{-# LANGUAGE TupleSections #-}
module Generator.Update (cUpdate) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils
import {-# SOURCE #-} CodeGenerator

import Data.List
import Control.Monad.Except


cUpdate :: Update -> CodeGenEnv
cUpdate (Update tbName equations whereCond) = filterMetadata ((==tbName) . metadata_name)
    >> getMetadata >>= \mds
    -> if null mds then throwError $ "no such table: " ++ tbName else doNothing
    >> let equations' = fixEquation (metadata_column $ head mds) []
           fixEquation (c:cs) res = fixEquation cs $ case findIndex ((==c) . fst) equations of
               Nothing -> Column c : res
               Just  i -> snd (equations !! i) : res
           fixEquation [] res = map (, "") $ reverse res

           filterCond x = iOpCode x `notElem` [opTransaction, opCommit, opVerifyCookie]

           locateCond a b = iOpCode a == opClose && iOpCode b == opRewind

           insertFromSelect = cInsertWrapper (Insert tbName [] $ SelectResult
                                            $ Select equations' [tbName] whereCond [] Nothing [] [])

           deleteWhereCond  = cDeleteWrapper (Delete tbName whereCond)

           checkCols (c:cs) = if snd (columnIdx c mds) == -1
                              then throwError $ "no such column: " ++ c
                              else checkCols cs
           checkCols [] = doNothing
        in checkCols (map fst equations)
        >> insertFromSelect >> insertEnvWhere2 (deleteWhereCond >> filterEnv filterCond) locateCond 
