{-# LANGUAGE LambdaCase #-}
module Generator.Index (cIndexAction) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils

import Data.List
import Control.Monad.Except


cIndexAction :: IndexAction -> CodeGenEnv
cIndexAction = \case
    ast@(CreateIndex idxName tbName idxDefs) -> getMetadata >>= \mds
        -> appendInstructions [Instruction opTransaction  0            0 ""
                              ,Instruction opVerifyCookie (cookie mds) 0 ""
                              ,Instruction opOpenWrite    0            0 "NMSqL_Master"]
        >> createIndex idxName tbName (show ast)
        >> appendInstructions [Instruction opClose     0 0 ""
                              ,Instruction opOpen      0 0 tbName
                              ,Instruction opOpenWrite 1 0 idxName
                              ,Instruction opRewind    0 0 ""
                              ,Instruction opNoop      0 1 ""]
        >> updateIndex mds
        >> appendInstructions [Instruction opNoop      0               2 ""
                              ,Instruction opNext      0               0 ""
                              ,Instruction opGoto      0               1 ""
                              ,Instruction opNoop      0               0 ""
                              ,Instruction opSetCookie (newCookie mds) 0 ""
                              ,Instruction opClose     0               0 ""
                              ,Instruction opClose     1               0 ""
                              ,Instruction opCommit    0               0 ""]
        where
            updateIndex mds = case filter (\md -> metadata_name md == tbName) mds of
                [md] -> let cols   = metadata_column md
                            idxes  = map (\df -> colIdx (fst df) cols) idxDefs
                         in connectCodeGenEnv (map idxToCol idxes)
                         >> appendInst opMakeKey (length idxes) 0 ""
                         >> appendInstructions [Instruction opAddress    0 0 ""
                                               ,Instruction opMakeRecord 1 0 ""
                                               ,Instruction opPut        1 0 ""]
                _ -> throwError $ "no such table: " ++ tbName

            colIdx col cols = case col `elemIndex` cols of
                Nothing -> Left $ "no such column: " ++ col
                Just  i -> Right i

            idxToCol idx = case idx of
                Right i -> appendInst opColumn 0 i ""
                Left er -> throwError er

    DropIndex idxName -> getMetadata >>= \mds
        -> if not $ idxExists idxName mds then throwError $ "no such index: " ++ idxName else doNothing
        >> appendInstructions
                [Instruction opTransaction  0                         0 ""
                ,Instruction opVerifyCookie (cookie mds)              0 ""
                ,Instruction opOpenWrite    0                         0 "NMSqL_Master"
                ,Instruction opRewind       0                         0 ""
                ,Instruction opNoop         0                         1 ""
                ,Instruction opString       0                         0 idxName
                ,Instruction opColumn       0                         1 ""
                ,Instruction opJNe          0                         2 ""
                ,Instruction opColumn       0                         3 ""
                ,Instruction opDelete       0                         0 ""
                ,Instruction opDestroy      0                         0 ""
                ,Instruction opGoto         0                         0 "" -- only one result, so we can break
                ,Instruction opNoop         0                         2 ""
                ,Instruction opNext         0                         0 ""
                ,Instruction opGoto         0                         1 ""
                ,Instruction opNoop         0                         0 ""
                ,Instruction opSetCookie    (nextCookie $ cookie mds) 0 ""
                ,Instruction opClose        0                         0 ""
                ,Instruction opCommit       0                         0 ""]


createIndex :: String -> String -> String -> CodeGenEnv
createIndex idxName tbName sql = getMetadata >>= \mds
    -> if   idxExists idxName mds
       then throwError $ "index " ++ idxName ++ " already exists"
       else appendInstructions
            [Instruction opDefaultKey  0 0 ""
            ,Instruction opString      0 0 "index"
            ,Instruction opString      0 0 idxName
            ,Instruction opString      0 0 tbName
            ,Instruction opCreateIndex 0 0 ""
            ,Instruction opString      0 0 sql
            ,Instruction opMakeRecord  5 0 ""
            ,Instruction opPut         0 0 ""]


idxExists :: String -> [TableMetadata] -> Bool
idxExists _ [] = False
idxExists idxName (md : mds) = (idxName `elem'` metadata_index md) || idxExists idxName mds where
    _ `elem'` [] = False
    a `elem'` (x:xs) = a == fst x || a `elem'` xs


-- help functions
cookie :: [TableMetadata] -> Int
cookie = metadata_cookie . head

newCookie :: [TableMetadata] -> Int
newCookie = nextCookie . cookie
