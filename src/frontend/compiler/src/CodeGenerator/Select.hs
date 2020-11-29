{-# LANGUAGE LambdaCase #-}
module Select where


import Ast
import Expr
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
data SelectResultType = ToSet | ToList | ToAgg | ToSorter | Normal

cSelect :: Select -> SelectResultType -> CodeGenEnv
cSelect select selType =
    let selRes = cSelRes $ selectResult select
     in undefined


cSelRes :: [(Expr, String)] -> SelectResultType -> CodeGenEnv
cSelRes [] _ = throwError "Select result can not be empty"

cSelRes selRes _ | not $ selResValid selRes = throwError "Semantic error on `*'"

cSelRes [(AnyColumn, _)] t =
    let getColumns = concatMap (\md -> map (TableColumn $ metadata_name md) (metadata_column md))
     in getMetadata >>= \mds -> cSelRes (map (\x -> (x, show x)) $ getColumns mds) t

cSelRes selRes ToList | length selRes > 1 =
    throwError "Only a single result allowed for a SELECT that is part of an expression"

cSelRes selRes _ =
    let
     in cSelRes' >> prependEnv (configAgg >> configOutput)
    where
        colCount = length selRes

        configOutput =
            let colNames = zipWith (\(a, b) idx -> appendInst opColumnName idx 0
                         $ if b == "" then show a else b) selRes [0..]
             in appendInst opColumnCount colCount 0 "" >> connectCodeGenEnv colNames

        configAgg = getAgg >>= \case
            0 -> getRes
            a -> appendInst opAggReset 0 a ""

        funcDef1 = [("max", 2), ("min", 2), ("substr", 3)]
        funcDef2 = [("max", 2), ("min", 2), ("substr", 3), ("count", 1), ("max", 1), ("min", 1)]

        cSelRes' = try (putFuncDef funcDef1 >> connectCodeGenEnv (map (cExpr . fst) selRes))
                       (connectCodeGenEnv (map (cExpr' . fst) selRes) >> applyCache)
            where
                toAgg = getAgg >>= \agg
                     -> appendInst opAggSet 0 agg ""
                     >> putCacheState 1
                     >> appendInst opAggGet 0 agg ""
                     >> putCacheState 0

                cExpr' e = try (putFuncDef funcDef1 >> cExpr e >> toAgg)
                               (putFuncDef funcDef2 >> cExpr e)


        putNmlRes env = insertTemp (env >> appendInst opCallback colCount 0 "")


----------------------------------------------------------
-- help functions
----------------------------------------------------------
selResValid :: [(Expr, b)] -> Bool
selResValid selRes =
    let anyColIdx = findIndex ((\case AnyColumn -> True; _ -> False) . fst) selRes
     in isNothing anyColIdx || length selRes == 1
