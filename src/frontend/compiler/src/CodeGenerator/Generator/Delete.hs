module Generator.Delete (cDelete) where

import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils
import {-# SOURCE #-} CodeGenerator

import Control.Monad.Except



cDelete :: Delete -> CodeGenEnv
cDelete (Delete tbName whereCond) = getMetadata >>= \mds'
    -> if not $ tbExists tbName mds' then throwError $ "no such table: " ++ tbName else doNothing
    >> putMetadata [head . dropWhile ((/=tbName) . metadata_name) $ mds']
    >> getMetadata >>= \mds
    -> appendInstructions [Instruction opTransaction  0            0 ""
                          ,Instruction opVerifyCookie (cookie mds) 0 ""]
    >> case whereCond of
        Nothing -> appendInstructions
                       [Instruction opOpen    0 0 "NMSqL_Master"
                       ,Instruction opRewind  0 0 ""
                       ,Instruction opNoop    0 1 ""
                       ,Instruction opString  0 0 tbName
                       ,Instruction opColumn  0 2 ""
                       ,Instruction opJNe     0 2 ""
                       ,Instruction opColumn  0 3 ""
                       ,Instruction opClear   0 0 ""
                       ,Instruction opNoop    0 2 ""
                       ,Instruction opNext    0 0 ""
                       ,Instruction opGoto    0 1 ""
                       ,Instruction opNoop    0 0 ""
                       ,Instruction opClose   0 0 ""]
        Just cond -> putWriteFlag True >> cExprWrapper cond
                  >> getCursorOpened >>= \co
                  -> insertTemp (deleteFromIndex idxs co >> appendInst opDelete 0 0 "")
                  >> putWriteFlag False where

            md   = head mds
            idxs = metadata_index md

            -- delete only refer to one table, so cursor number must be 1
            -- co is cursor name opened
            deleteFromIndex [] co =
                if   tbName `elem` co
                then doNothing
                else getLabel >>= \lab ->
                     appendInstructions
                        [Instruction opOpenWrite 1  0         tbName
                        ,Instruction opRewind    1  0         ""
                        ,Instruction opNoop      0  (lab + 1) ""
                        ,Instruction opAddress   0  0         ""
                        ,Instruction opAddress   1  0         ""
                        ,Instruction opJNe       0  (lab + 2) ""
                        ,Instruction opDelete    1  0         ""
                        -- only one result, so we can break
                        ,Instruction opGoto      0  lab       ""
                        ,Instruction opNoop      0  (lab + 2) ""
                        ,Instruction opNext      0  lab       ""
                        ,Instruction opGoto      0  (lab + 1) ""
                        ,Instruction opNoop      0  lab       ""
                        ,Instruction opClose     1  0         ""] >>
                     putLabel (lab + 3)
            deleteFromIndex ((name, colNames):xs) co =
                if   name `elem` co
                then doNothing >> deleteFromIndex xs co
                else let idx = map (snd . (`columnIdx` mds)) colNames
                         key = appendInstructions (map (\i -> Instruction opColumn 0 i "") idx)
                            >> appendInst opMakeKey  (length colNames) 0 ""
                            >> appendInst opBeginIdx 1                 0 ""
                      in appendInst opOpenWrite 1 0 name >> key >> getLabel >>= \lab
                      -> appendInstructions
                            [Instruction opNoop    0  (lab + 1) ""
                            ,Instruction opAddress 0  0         ""
                            ,Instruction opAddress 1  0         ""
                            ,Instruction opJNe     0  (lab + 2) ""
                            ,Instruction opDelete  1  0         ""
                            -- only one result, so we can break
                            ,Instruction opGoto    0  lab       ""
                            ,Instruction opNoop    0  (lab + 2) ""
                            ,Instruction opNextIdx 0  lab       ""
                            ,Instruction opGoto    0  (lab + 1) ""
                            ,Instruction opNoop    0  lab       ""
                            ,Instruction opClose   1  0         ""]
                      >> putLabel (lab + 3)
                      >> deleteFromIndex xs co
    >> appendInst opCommit 0 0 ""