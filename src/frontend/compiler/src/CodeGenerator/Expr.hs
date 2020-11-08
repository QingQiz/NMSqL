{-# LANGUAGE LambdaCase #-}
module Expr (cExpr) where


import Ast
import Instruction
import FFIStructure
import CodeGeneratorUtils

import Data.List
import Data.Maybe
import Control.Monad.Except


----------------------------------------------------------
-- Code Generator
----------------------------------------------------------
cExpr :: Expr -> CodeGenEnv
cExpr expr = do
    case expr of
        BinExpr op e1 e2
            | op == And                                 -> exprAnd e1 e2
            | op == Or                                  -> exprOr  e1 e2
            | op `elem` [Plus, Minus, Divide, Multiple] -> exprArith op e1 e2
            | otherwise                                 -> exprCompr op e1 e2
        LikeExpr op e1 e2                               -> exprLike  op e1 e2
        ConstValue val                                  -> exprConst val
        FunctionCall fn pl                              -> exprFuncCall fn pl
        IsNull e                                        -> cExpr e >> trueAndMkRes opIsNull
        Between e1 e2 e3                                -> cExpr $ BinExpr And (BinExpr Ls e1 e3) (BinExpr Gt e1 e2)
        InExpr _ _                                      -> throwError "not implemented"
        NotExpr e                                       -> cExpr e >> appendInst (Instruction opNot 0 0 "")
        SelectExpr _                                    -> throwError "not implemented"
        Column cn                                       -> exprColumn cn
        TableColumn tn cn                               -> exprTableColumn tn cn
        _                                               -> throwError "Semantic error"
    retRes


-- code generator for column
exprColumn :: String -> CodeGenEnv
exprColumn cn =
    let valid     md = cn `elem` (metadata_column md)
        getColIdx md = fromMaybe (-1) $ findIndex (==cn) $ metadata_column md
     in getMetadata >>= return . findIndices valid >>= \case
            [i] -> getMetadata >>= (\md -> return $ getColIdx $ md !! i)
                               >>= (\j  -> appendInst $ Instruction opColumn i j "")
            []  -> throwError $ "No such column: " ++ cn
            _   -> throwError $ "Ambiguous column name: " ++ cn


-- code generator for table-column
exprTableColumn :: String -> String -> CodeGenEnv
exprTableColumn tn cn =
    let tbIdx = getMetadata >>= return . findIndex ((==tn) . metadata_name)
     in tbIdx >>= \case
            Nothing -> throwError $ "No such column: " ++ tn ++ "." ++ cn
            Just  i -> getMetadata >>= (\x -> return $ findIndex (==cn) $ metadata_column $ x !! i) >>= \case
                Nothing -> throwError $ "No such column: " ++ tn ++ "." ++ cn
                Just  j -> appendInst $ Instruction opColumn i j ""


-- code generator for function-call-expr
exprFuncCall :: String -> [Expr] -> CodeGenEnv
exprFuncCall fn pl  =
    let fnChecker   = [("max", 2, opMax), ("min", 2, opMin)]
        plLength    = length pl
        pl'         = map cExpr pl
     in case dropWhile ((fn/=) . fst3) fnChecker of
             []  -> throwError $ "No such function: " ++ fn
             x:_ | plLength < snd3 x -> throwError $ "Too few arguments to function: "  ++ fn
                 | plLength < snd3 x -> throwError $ "Too many arguments to function: " ++ fn
                 | otherwise -> foldr (>>) retRes pl'
                             >> appendInst (Instruction (trd3 x) 0 0 "")


-- code generator for const-expr
exprConst :: Value -> CodeGenEnv
exprConst val = case val of
    ValStr str       -> appendInst $ Instruction opString  0   0 str
    ValInt int       -> appendInst $ Instruction opInteger int 0 ""
    ValDouble double -> appendInst $ Instruction opString  0   0 $ show double


-- code generator for like-expr
exprLike :: LikeOp -> Expr -> Expr -> CodeGenEnv
exprLike op e1 e2  = cExpr e1 >> cExpr e2 >> cLikeOp  op


-- code generator for arith-expr
exprArith :: BinOp -> Expr -> Expr -> CodeGenEnv
exprArith op e1 e2 = cExpr e1 >> cExpr e2 >> cArithOp op


-- code generator for compare-expr
exprCompr :: BinOp -> Expr -> Expr -> CodeGenEnv
exprCompr op e1 e2 = cExpr e1 >> cExpr e2 >> cComprOp op


-- code generator for and-expr
exprAnd :: Expr -> Expr -> CodeGenEnv
exprAnd e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 >> gotoWhenFalse labelAva
    >> cExpr e2 >> gotoWhenFalse labelAva
    >> putTrue  >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putFalse
    >> getLabel >>= mkLabel


-- code generator for or-expr
exprOr :: Expr -> Expr -> CodeGenEnv
exprOr e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 >> gotoWhenTrue labelAva
    >> cExpr e2 >> gotoWhenTrue labelAva
    >> putFalse >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putTrue
    >> getLabel >>= mkLabel


-- code generator for like/glob operators
cLikeOp :: LikeOp -> CodeGenEnv
cLikeOp op = case op of
    Like    -> trueAndMkRes  opLike
    Glob    -> trueAndMkRes  opGlob
    NotLike -> falseAndMkRes opLike
    NotGlob -> falseAndMkRes opGlob

-- code generator for arithmetic operators
cArithOp :: BinOp -> CodeGenEnv
cArithOp op = case op of
    Plus     -> appendInst $ Instruction opAdd      0 0 ""
    Minus    -> appendInst $ Instruction opSubtract 0 0 ""
    Multiple -> appendInst $ Instruction opMultiply 0 0 ""
    Divide   -> appendInst $ Instruction opDivide   0 0 ""


-- code generator for comparison operators
cComprOp :: BinOp -> CodeGenEnv
cComprOp op =
    let chart = [
            (Ls, opLt), (LE, opLe), (Gt, opGt), (GE, opGe),
            (Eq, opEq), (NE, opNe)]
     in trueAndMkRes . snd . head . dropWhile ((op /=) . fst) $ chart


----------------------------------------------------------
-- help functions
----------------------------------------------------------
-- make goto instruction
goto :: Int -> CodeGenEnv
goto          label = appendInst $ Instruction opGoto 0 label ""

gotoWhenTrue :: Int -> CodeGenEnv
gotoWhenTrue  label = appendInst $ Instruction opIf   0 label ""

gotoWhenFalse :: Int -> CodeGenEnv
gotoWhenFalse label = appendInst  (Instruction opNot  0 0     "") >> gotoWhenTrue label


-- make label
mkLabelWithoutUpdate :: Int -> CodeGenEnv
mkLabelWithoutUpdate label = appendInst (Instruction opNoop 0 label "")

mkLabel :: Int -> CodeGenEnv
mkLabel label  = mkLabelWithoutUpdate label >> updateLabel

mkCurrentLabel :: CodeGenEnv
mkCurrentLabel = getLabel >>= mkLabel


-- put true/false to stack
putTrue :: CodeGenEnv
putTrue  = appendInst $ Instruction opInteger 1 0 ""

putFalse :: CodeGenEnv
putFalse = appendInst $ Instruction opInteger 0 0 ""


-- put result to stack after insturction is executed
-- opCode then goto true or fallback to false
mkBoolVal :: OpCode -> Int -> Int -> String -> CodeGenEnv
mkBoolVal opCode p1 p2 p3 = appendInst (Instruction opCode p1 p2 p3)
    >> putFalse
    >> goto (p2 + 1)
    >> mkCurrentLabel
    >> putTrue
    >> mkCurrentLabel

trueAndMkRes :: OpCode -> CodeGenEnv
trueAndMkRes  opCode = getLabel >>= \l -> mkBoolVal opCode 0 l ""

falseAndMkRes :: OpCode -> CodeGenEnv
falseAndMkRes opCode = getLabel >>= \l -> mkBoolVal opCode 1 l ""
