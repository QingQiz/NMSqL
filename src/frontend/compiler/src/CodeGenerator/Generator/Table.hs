{-# LANGUAGE LambdaCase #-}
module Generator.Table (cTableAction) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils


cTableAction :: TableActon -> CodeGenEnv
cTableAction = \case
    ast@(CreateTable name colDef tbCtt) -> getMetadata >>= (\cookie
        -> appendInstructions [Instruction opTransaction  0      0 ""
                              ,Instruction opVerifyCookie cookie 0 ""
                              ,Instruction opOpenWrite    0      0 "NMSqL_Master"]
        >> connectCodeGenEnv (map (\i -> createIdx (mkIdxName i) name) [1..idxNum])
        >> createTable name (show ast)
        >> appendInstructions [Instruction opSetCookie (nextCookie cookie) 0 ""
                              ,Instruction opClose     0                   0 ""
                              ,Instruction opCommit    0                   0 ""]
        ) . metadata_cookie . head
        where
            idxNum =
                let validColCtt = \case ColPrimaryKey {} -> True; ColUnique -> True; _ -> False
                    getColCttCnt (ColumnDef _ _ ctts) = length $ filter validColCtt ctts
                    getTbCttCnt = sum $ map (\case TbPrimaryKey x -> length x; TbUnique x -> length x; _ -> 0) tbCtt
                 in sum (map getColCttCnt colDef) + getTbCttCnt
            mkIdxName i = "(" ++ name ++ " autoindex " ++ show i ++ ")"

    DropTable tbName -> getMetadata >>= (\cookie
        -> appendInstructions [Instruction opTransaction  0      0 ""
                              ,Instruction opVerifyCookie cookie 0 ""
                              ,Instruction opSetCookie    (nextCookie cookie) 0 ""
                              ,Instruction opOpenWrite    0      0 "NMSqL_Master"
                              ,Instruction opRewind       0      0 ""
                              ,Instruction opNoop         0      1 ""
                              ,Instruction opString       0      0 tbName
                              ,Instruction opColumn       0      2 ""
                              ,Instruction opJNe          0      2 ""
                              ,Instruction opColumn       0      3 ""
                              ,Instruction opDestroy      0      0 ""
                              ,Instruction opNoop         0      2 ""
                              ,Instruction opNext         0      0 ""
                              ,Instruction opGoto         0      1 ""
                              ,Instruction opNoop         0      0 ""
                              ,Instruction opClose        0      0 ""
                              ,Instruction opCommit       0      0 ""]
            ) . metadata_cookie . head


createIdx :: String -> String -> CodeGenEnv
createIdx idxName tableName = appendInstructions
    [Instruction opNewRecno    0 0 ""
    ,Instruction opString      0 0 "index"
    ,Instruction opString      0 0 idxName
    ,Instruction opString      0 0 tableName
    ,Instruction opCreateIndex 0 0 ""
    ,Instruction opNull        0 0 ""
    ,Instruction opMakeRecord  5 0 ""
    ,Instruction opPut         0 0 ""]


createTable :: String -> String -> CodeGenEnv
createTable tbName sql = appendInstructions
    [Instruction opNewRecno    0 0 ""
    ,Instruction opString      0 0 "index"
    ,Instruction opString      0 0 tbName
    ,Instruction opString      0 0 tbName
    ,Instruction opCreateIndex 0 0 ""
    ,Instruction opString      0 0 sql
    ,Instruction opMakeRecord  5 0 ""
    ,Instruction opPut         0 0 ""]

