{-# LANGUAGE LambdaCase #-}
module Generator.Table (cTableAction) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils

import Control.Monad.Except


cTableAction :: TableActon -> CodeGenEnv
cTableAction = \case
    ast@(CreateTable name colDef tbCtt) -> getMetadata >>= (\c
        -> appendInstructions [Instruction opTransaction  0 0 ""
                              ,Instruction opVerifyCookie c 0 ""
                              ,Instruction opOpenWrite    0 0 "NMSqL_Master"]
        >> connectCodeGenEnv (map (\i -> createIdx (mkIdxName i) name) [1..idxNum])
        >> createTable name (show ast)
        >> appendInstructions [Instruction opSetCookie (nextCookie c) 0 ""
                              ,Instruction opClose     0              0 ""
                              ,Instruction opCommit    0              0 ""]
        ) . metadata_cookie . head
        where
            idxNum =
                let validColCtt = \case ColPrimaryKey {} -> True; ColUnique -> True; _ -> False
                    getColCttCnt (ColumnDef _ _ ctts) = length $ filter validColCtt ctts
                    getTbCttCnt = sum $ map (\case TbPrimaryKey x -> length x; TbUnique x -> length x; _ -> 0) tbCtt
                 in sum (map getColCttCnt colDef) + getTbCttCnt
            mkIdxName i = "(" ++ name ++ " autoindex " ++ show i ++ ")"

    DropTable tbName -> getMetadata >>= \mds
        -> if   tbExists tbName mds
           then appendInstructions
                [Instruction opTransaction  0                         0 ""
                ,Instruction opVerifyCookie (cookie mds)              0 ""
                ,Instruction opOpenWrite    0                         0 "NMSqL_Master"
                ,Instruction opRewind       0                         0 ""
                ,Instruction opNoop         0                         1 ""
                ,Instruction opString       0                         0 tbName
                ,Instruction opColumn       0                         2 ""
                ,Instruction opJNe          0                         2 ""
                ,Instruction opColumn       0                         3 ""
                ,Instruction opDelete       0                         0 ""
                ,Instruction opDestroy      0                         0 ""
                ,Instruction opNoop         0                         2 ""
                ,Instruction opNext         0                         0 ""
                ,Instruction opGoto         0                         1 ""
                ,Instruction opNoop         0                         0 ""
                ,Instruction opSetCookie    (nextCookie $ cookie mds) 0 ""
                ,Instruction opClose        0                         0 ""
                ,Instruction opCommit       0                         0 ""]
           else throwError $ "no such table: " ++ tbName


-- -----------------------------------------
-- | NMSqL_Master                          |
-- -----------------------------------------
-- | type | key | tableName | page | value |
-- -----------------------------------------

createIdx :: String -> String -> CodeGenEnv
createIdx idxName tableName = appendInstructions
    [Instruction opDefaultKey  0 0 ""
    ,Instruction opString      0 0 "index"
    ,Instruction opString      0 0 idxName
    ,Instruction opString      0 0 tableName
    ,Instruction opCreateIndex 0 0 ""
    ,Instruction opNull        0 0 ""
    ,Instruction opMakeRecord  5 0 ""
    ,Instruction opPut         0 0 ""]


createTable :: String -> String -> CodeGenEnv
createTable tbName sql = getMetadata >>= \mds
    -> if   tbExists tbName mds
       then throwError $ "table " ++ tbName ++ " already exists"
       else appendInstructions
            [Instruction opDefaultKey  0 0 ""
            ,Instruction opString      0 0 "table"
            ,Instruction opString      0 0 tbName
            ,Instruction opString      0 0 tbName
            ,Instruction opCreateTable 0 0 ""
            ,Instruction opString      0 0 sql
            ,Instruction opMakeRecord  5 0 ""
            ,Instruction opPut         0 0 ""]