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
    >> filterMetadata ((==tbName) . metadata_name)
    >> getMetadata >>= \mds
    -> appendInstructions [Instruction opTransaction  0            0 ""
                          ,Instruction opVerifyCookie (cookie mds) 0 ""]
    >> case whereCond of
        Nothing -> getCursor >>= \cr -> appendInstructions
                       [Instruction opOpen    cr 0 "NMSqL_Master"
                       ,Instruction opRewind  cr 0 ""
                       ,Instruction opNoop    0  1 ""
                       ,Instruction opString  0  0 tbName
                       ,Instruction opColumn  cr 2 ""
                       ,Instruction opJNe     0  2 ""
                       ,Instruction opColumn  cr 3 ""
                       ,Instruction opClear   0  0 ""
                       ,Instruction opNoop    0  2 ""
                       ,Instruction opNext    cr 0 ""
                       ,Instruction opGoto    0  1 ""
                       ,Instruction opNoop    0  0 ""
                       ,Instruction opClose   cr 0 ""] >> putCursor (cr + 1)
        Just cond -> putWriteFlag True >> cExprWrapper cond
                  >> getCursorOpened >>= \co
                  -> getCursor >>= \cr
                  -> insertTemp (deleteFromIndex idxs co >> appendInst opDelete (cr - 1) 0 "")
                  >> putCursor (cr + 1)
                  >> putWriteFlag False where

            md   = head mds
            idxs = metadata_index md

            -- co is cursor name opened
            deleteFromIndex [] co = getCursor >>= \cr' -> let cr = cr' - 1 in
                if   tbName `elem` co
                then doNothing
                else getLabel >>= \lab ->
                     appendInstructions
                        [Instruction opOpenWrite cr' 0         tbName
                        ,Instruction opRewind    cr' 0         ""
                        ,Instruction opNoop      0   (lab + 1) ""
                        ,Instruction opAddress   cr  0         ""
                        ,Instruction opAddress   cr' 0         ""
                        ,Instruction opJNe       0   (lab + 2) ""
                        ,Instruction opDelete    cr' 0         ""
                        -- only one result, so we can break
                        ,Instruction opGoto      0   lab       ""
                        ,Instruction opNoop      0   (lab + 2) ""
                        ,Instruction opNext      cr' lab       ""
                        ,Instruction opGoto      0   (lab + 1) ""
                        ,Instruction opNoop      0   lab       ""
                        ,Instruction opClose     cr' 0         ""] >> putLabel (lab + 3)

            deleteFromIndex ((name, colNames):xs) co = getCursor >>= \cr' -> let cr = cr' - 1 in
                if   name `elem` co
                then doNothing >> deleteFromIndex xs co
                else let idx = map (snd . (`columnIdx` mds)) colNames
                         key = appendInstructions (map (\i -> Instruction opColumn 0 i "") idx)
                            >> appendInst opMakeKey  (length colNames) 0 ""
                            >> appendInst opBeginIdx cr'                 0 ""
                      in appendInst opOpenWrite cr' 0 name >> key >> getLabel >>= \lab
                      -> appendInstructions
                            [Instruction opNoop    0   (lab + 1) ""
                            ,Instruction opAddress cr  0         ""
                            ,Instruction opAddress cr' 0         ""
                            ,Instruction opJNe     0   (lab + 2) ""
                            ,Instruction opDelete  cr' 0         ""
                            -- only one result, so we can break
                            ,Instruction opGoto    0   lab       ""
                            ,Instruction opNoop    0   (lab + 2) ""
                            ,Instruction opNextIdx cr' lab       ""
                            ,Instruction opGoto    0   (lab + 1) ""
                            ,Instruction opNoop    0   lab       ""
                            ,Instruction opClose   cr'  0         ""]
                      >> putLabel (lab + 3)
                      >> deleteFromIndex xs co
    >> appendInst opCommit 0 0 ""