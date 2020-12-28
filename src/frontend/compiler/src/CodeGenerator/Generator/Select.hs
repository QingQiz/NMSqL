{-# LANGUAGE LambdaCase #-}
module Generator.Select (cSelect) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils
import {-# SOURCE #-} CodeGenerator

import Generator.Expr (cExpr)

import Data.List
import Data.Maybe
import Control.Monad.Except


----------------------------------------------------------
-- Code Generator
----------------------------------------------------------
cSelect :: Select -> SelectResultType -> CodeGenEnv
cSelect select selType =
    let selRes       = selectResult select
        selWhere     = fromMaybe EmptyExpr (selectWhere select)
     in filterMetadata (\md -> metadata_name md `elem` selectTableName select)
     >> cExprWrapper selWhere
     >> cSelRes selRes selType
     >> resetMetadata
     >> getRes


cSelRes :: [(Expr, String)] -> SelectResultType -> CodeGenEnv
cSelRes [] _ = throwError "Select result can not be empty"

cSelRes selRes _ | not selResValid = throwError "Semantic error on `*'" where
    selResValid =
        let anyColIdx = findIndex ((\case AnyColumn -> True; _ -> False) . fst) selRes
         in isNothing anyColIdx || length selRes == 1

cSelRes [(AnyColumn, _)] t =
    let getColumns = concatMap (\md -> map (TableColumn $ metadata_name md) (metadata_column md))
     in getMetadata >>= \mds -> cSelRes (map (\x -> (x, show x)) $ getColumns mds) t

cSelRes selRes (ToSet _) | length selRes > 1 =
    throwError "Only a single result allowed for a SELECT that is part of an expression"

cSelRes selRes (UnionSel op n) | length selRes /= n =
    throwError $ "SELECTs to the left and right of " ++ show op ++ " do not have the same number of result columns"

cSelRes selRes selType = cSelRes' >> putSelRes >> configOutput where
    configOutput =
        let colNames = zipWith (\(a, b) idx -> appendInst opColumnName idx 0
                     $ if b == "" then show a else b) selRes [0..]
         in case selType of
            Normal -> prependEnv $
                      appendInst opColumnCount colNr 0 "" >> connectCodeGenEnv colNames
            _      -> doNothing

    putSelRes = case selType of
        ToSet set -> prependEnv (appendInst opSetOpen   set   0 "")
                  >> insertTemp (appendInst opSetInsert set   0 "")
        Normal    -> getAgg >>= \case
            0 -> insertTemp (appendInst opCallback  colNr 0 "")
            a -> prependEnv (appendInst opAggReset 0 a "")
              >> appendInst opCallback colNr 0 ""
        -- same as Normal but replace opCallback with opMakeRecord and opPut
        ToTemp -> getCursor >>= \cr -> putCursor (cr + 1)
               >> prependEnv (appendInst opOpenTemp cr 0 "")
               >> getAgg >>= (\case
                      0 -> insertTemp (appendInstructions
                               [Instruction opMakeRecord colNr 0 ""
                               ,Instruction opDefaultKey cr    0 ""
                               ,Instruction opDup        1     0 ""
                               ,Instruction opPut        cr    0 ""
                               ,Instruction opPop        1     0 ""])
                      a -> prependEnv (appendInst opAggReset 0 a "")
                        >> appendInstructions
                               [Instruction opMakeRecord colNr 0 ""
                               ,Instruction opDefaultKey cr    0 ""
                               ,Instruction opDup        1     0 ""
                               ,Instruction opPut        cr    0 ""
                               ,Instruction opPop        1     0 ""])

        ToSorter  -> throwError "TODO Not implemented"
        UnionSel _ _  -> throwError "TODO Not implemented"

    colNr = length selRes

    cSelRes' = try (putFuncDef funcDef1 >> insertTemp (connectCodeGenEnv $ map (cExpr . fst) selRes))
                   (insertTemp (connectCodeGenEnv $ map (cExpr' . fst) selRes) >> applyCache)
        where
            funcDef1 = [("max", 2), ("min", 2), ("substr", 3)]
            funcDef2 = [("max", 2), ("min", 2), ("substr", 3), ("count", 1), ("max", 1), ("min", 1)]

            toAgg = getAgg >>= \agg -> updateAgg
                 >> appendInst opAggSet 0 agg ""
                 >> putCacheState 1
                 >> appendInst opAggGet 0 agg ""
                 >> putCacheState 0

            cExpr' e = try (putFuncDef funcDef1 >> cExpr e >> toAgg)
                           (putFuncDef funcDef2 >> cExpr e >> putCacheState 0)