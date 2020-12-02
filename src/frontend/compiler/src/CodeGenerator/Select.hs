{-# LANGUAGE LambdaCase #-}
module Select (cSelect) where


import Ast
import Expr ( cExpr )
import Instruction
import FFIStructure
import CodeGeneratorUtils
import {-# SOURCE #-} CodeGenerator

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
        tableName    = selectTableName select
        newMd        = getMetadata >>= \mds ->
                       putMetadata $ filter (\md -> metadata_name md `elem` tableName) mds
        verifyCookie = getMetadata >>= \mds -> appendInst opVerifyCookie (metadata_cookie (head mds)) 0 ""
     in newMd >> verifyCookie >> cExprWrapper selWhere >> getRes >> cSelRes selRes selType


cSelRes :: [(Expr, String)] -> SelectResultType -> CodeGenEnv
cSelRes [] _ = throwError "Select result can not be empty"

cSelRes selRes _ | not $ selResValid selRes = throwError "Semantic error on `*'"

cSelRes [(AnyColumn, _)] t =
    let getColumns = concatMap (\md -> map (TableColumn $ metadata_name md) (metadata_column md))
     in getMetadata >>= \mds -> cSelRes (map (\x -> (x, show x)) $ getColumns mds) t

cSelRes selRes (ToSet _) | length selRes > 1 =
    throwError "Only a single result allowed for a SELECT that is part of an expression"

cSelRes selRes (UnionSel op n) | length selRes /= n =
    throwError $ "SELECTs to the left and right of " ++ show op ++ " do not have the same number of result columns"

cSelRes selRes selType =
    let
     in cSelRes' >> putSelRes >> prependEnv (configAgg >> configOutput)
    where
        colNr = length selRes

        configOutput =
            let colNames = zipWith (\(a, b) idx -> appendInst opColumnName idx 0
                         $ if b == "" then show a else b) selRes [0..]
             in appendInst opColumnCount colNr 0 "" >> connectCodeGenEnv colNames

        configAgg = getAgg >>= \case
            0 -> doNothing
            a -> appendInst opAggReset 0 a ""

        putSelRes = case selType of
            ToSet set -> prependEnv (appendInst opSetOpen   set   0 "")
                      >> insertTemp (appendInst opSetInsert set   0 "")
            Normal    -> insertTemp (appendInst opCallback  colNr 0 "")
            ToSorter  -> throwError "TODO Not implemented"
            UnionSel _ _  -> throwError "TODO Not implemented"

        cSelRes' = try (putFuncDef funcDef1 >> insertTemp (connectCodeGenEnv (map (cExpr . fst) selRes)))
                       (connectCodeGenEnv (map (cExpr' . fst) selRes) >> applyCache)
            where
                funcDef1 = [("max", 2), ("min", 2), ("substr", 3)]
                funcDef2 = [("max", 2), ("min", 2), ("substr", 3), ("count", 1), ("max", 1), ("min", 1)]

                toAgg = getAgg >>= \agg
                     -> appendInst opAggSet 0 agg ""
                     >> putCacheState 1
                     >> appendInst opAggGet 0 agg ""
                     >> putCacheState 0

                cExpr' e = try (putFuncDef funcDef1 >> cExpr e >> toAgg)
                               (putFuncDef funcDef2 >> cExpr e)


----------------------------------------------------------
-- help functions
----------------------------------------------------------
selResValid :: [(Expr, b)] -> Bool
selResValid selRes =
    let anyColIdx = findIndex ((\case AnyColumn -> True; _ -> False) . fst) selRes
     in isNothing anyColIdx || length selRes == 1
