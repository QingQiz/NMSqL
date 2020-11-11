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
cExpr expr = case expr of
    BinExpr op e1 e2
        | op == And                                 -> exprAnd e1 e2
        | op == Or                                  -> exprOr  e1 e2
        | op `elem` [Plus, Minus, Divide, Multiply] -> exprArith op e1 e2
        | otherwise                                 -> exprCompr op e1 e2
    LikeExpr op e1 e2                               -> exprLike  op e1 e2
    ConstValue val                                  -> exprConst val
    FunctionCall fn pl                              -> exprFuncCall fn pl
    IsNull e                                        -> cExpr e >> appendInst (Instruction opSetIsNull 0 1 "")
    Between e1 e2 e3                                -> exprBetween e1 e2 e3
    InExpr a b                                      -> exprIn a b
    NotExpr e                                       -> cExpr e >> appendInst (Instruction opNot 0 0 "")
    SelectExpr _                                    -> throwError "not implemented"
    Column cn                                       -> exprColumn cn
    TableColumn tn cn                               -> exprTableColumn tn cn
    _                                               -> throwError "Semantic error"


-- code generator for between-expr
exprBetween :: Expr -> Expr -> Expr -> CodeGenEnv
exprBetween a b c = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr a >> dup                                            -- stack: a,a
    >> cExpr c >> appendInst (Instruction opJGe 0 labelAva "")   -- stack: a,a,c -> a
    >> dup                                                       -- stack: a,a
    >> cExpr b >> appendInst (Instruction opJLe 0 labelAva "")   -- stack: a,a,b -> a
    >> pop 1 >> putTrue
    >> getLabel >>= goto
    >> mkLabel' labelAva
    >> pop 1 >> putFalse
    >> mkCurrentLabel


-- code generator for in-expr
exprIn :: Expr -> ValueList -> CodeGenEnv
exprIn a (ValueList vl) =
    let genValueList = do
            oldRes <- getRes
            insSet <- (>>) (putRes []) $ foldr (>>) (getRes)
                          $ map (\x -> throwErrorIfNotConst x
                                    >> cExpr x
                                    >> getSet >>= \sn
                                    -> appendInst (Instruction opSetInsert sn 0 "")) vl
            putRes $ insSet ++ oldRes
        mkRes = do
            lab <- getLabel
            set <- getSet
            mkBoolVal opSetFound set lab ""
     in case vl of
            [] -> putFalse
            _  -> genValueList >> cExpr a >> mkRes >> updateSet

exprIn _ (SelectResult _) = throwError "not implemented"


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
                 | plLength > snd3 x -> throwError $ "Too many arguments to function: " ++ fn
                 | otherwise -> foldr (>>) getRes pl'
                             >> appendInst (Instruction (trd3 x) 0 0 "")


-- code generator for const-expr
exprConst :: Value -> CodeGenEnv
exprConst val = case val of
    ValStr str       -> appendInst $ Instruction opString  0   0 str
    ValInt int       -> appendInst $ Instruction opInteger int 0 ""
    ValDouble double -> appendInst $ Instruction opString  0   0 $ show double
    Null             -> appendInst $ Instruction opNull    0   0 ""


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
    >> mkLabel' labelAva
    >> putFalse
    >> mkCurrentLabel


-- code generator for or-expr
exprOr :: Expr -> Expr -> CodeGenEnv
exprOr e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 >> gotoWhenTrue labelAva
    >> cExpr e2 >> gotoWhenTrue labelAva
    >> putFalse >> getLabel >>= goto
    >> mkLabel' labelAva
    >> putTrue
    >> mkCurrentLabel


-- code generator for like/glob operators
cLikeOp :: LikeOp -> CodeGenEnv
cLikeOp op = case op of
    Like    -> appendInst $ Instruction opSetLike 0 1 ""
    Glob    -> appendInst $ Instruction opSetGlob 0 1 ""
    NotLike -> appendInst $ Instruction opSetLike 1 1 ""
    NotGlob -> appendInst $ Instruction opSetGlob 1 1 ""


-- code generator for arithmetic operators
cArithOp :: BinOp -> CodeGenEnv
cArithOp op = case op of
    Plus     -> appendInst $ Instruction opAdd      0 0 ""
    Minus    -> appendInst $ Instruction opSubtract 0 0 ""
    Multiply -> appendInst $ Instruction opMultiply 0 0 ""
    Divide   -> appendInst $ Instruction opDivide   0 0 ""
    _        -> throwError "Wrong op type"


-- code generator for comparison operators
cComprOp :: BinOp -> CodeGenEnv
cComprOp op =
    let chart = [
            (Ls, opSetLt), (LE, opSetLe), (Gt, opSetGt), (GE, opSetGe), (Eq, opSetEq), (NE, opSetNe)]
     in (\x -> appendInst $ Instruction x 0 1 "") . snd . head . dropWhile ((op /=) . fst) $ chart


----------------------------------------------------------
-- help functions
----------------------------------------------------------
-- make goto instruction
goto :: Int -> CodeGenEnv
goto label = appendInst $ Instruction opGoto 0 label ""

gotoWhenTrue :: Int -> CodeGenEnv
gotoWhenTrue label = appendInst $ Instruction opJIf 0 label ""

gotoWhenFalse :: Int -> CodeGenEnv
gotoWhenFalse label = appendInst (Instruction opNot  0 0 "") >> gotoWhenTrue label


-- make label without update
mkLabel' :: Int -> CodeGenEnv
mkLabel' label = appendInst (Instruction opNoop 0 label "")

-- make label and update label number
mkLabel :: Int -> CodeGenEnv
mkLabel label  = mkLabel' label >> updateLabel

mkCurrentLabel :: CodeGenEnv
mkCurrentLabel = getLabel >>= mkLabel

dup :: CodeGenEnv
dup = appendInst $ Instruction opDup 0 0 ""

pop :: Int -> CodeGenEnv
pop cnt = appendInst $ Instruction opPop cnt 0 ""

-- put true/false to stack
putTrue :: CodeGenEnv
putTrue = appendInst $ Instruction opInteger 1 0 ""

putFalse :: CodeGenEnv
putFalse = appendInst $ Instruction opInteger 0 0 ""


mkBoolVal :: OpCode -> Int -> Int -> String -> CodeGenEnv
mkBoolVal opCode p1 p2 p3 = appendInst (Instruction opCode p1 p2 p3)
    >> putFalse
    >> goto (p2 + 1)
    >> mkCurrentLabel
    >> putTrue
    >> mkCurrentLabel

throwErrorIfNotConst :: Expr -> CodeGenEnv
throwErrorIfNotConst expr =
    case expr of
        BinExpr  _ a b         -> throwErrorIfNotConst a >> throwErrorIfNotConst b
        LikeExpr _ a b         -> throwErrorIfNotConst a >> throwErrorIfNotConst b
        ConstValue _           -> return []
        FunctionCall _ a       -> foldr (>>) (return []) (map throwErrorIfNotConst a)
        IsNull a               -> throwErrorIfNotConst a
        Between a b c          -> throwErrorIfNotConst a >> throwErrorIfNotConst b
                               >> throwErrorIfNotConst c
        InExpr a (ValueList b) -> throwErrorIfNotConst a
                               >> foldr (>>) (return []) (map throwErrorIfNotConst b)
        NotExpr a              -> throwErrorIfNotConst a
        _                      -> throwError "Right-hand side of IN operator must be constant"