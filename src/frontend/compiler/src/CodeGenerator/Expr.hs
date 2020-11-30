{-# LANGUAGE LambdaCase #-}
module Expr (cExpr, isConstExpr) where


import Ast
import Instruction
import CodeGeneratorUtils

import Control.Monad.Except


----------------------------------------------------------
-- Code Generator
----------------------------------------------------------
cExpr :: Expr -> CodeGenEnv
cExpr = \case
    BinExpr op e1 e2
        | op == And                                 -> exprAnd e1 e2
        | op == Or                                  -> exprOr  e1 e2
        | op `elem` [Plus, Minus, Divide, Multiply] -> exprArith op e1 e2
        | otherwise                                 -> exprCompr op e1 e2
    LikeExpr op e1 e2                               -> exprLike  op e1 e2
    ConstValue val                                  -> exprConst val
    FunctionCall fn pl                              -> exprFuncCall fn pl
    IsNull e                                        -> cExpr e >> appendInst opSetIsNull 0 1 ""
    Between e1 e2 e3                                -> exprBetween e1 e2 e3
    InExpr a b                                      -> exprIn a b
    NotExpr e                                       -> cExpr e >> appendInst opNot 0 0 ""
    SelectExpr _                                    -> throwError "not implemented"
    Column cn                                       -> exprColumn cn
    TableColumn tn cn                               -> exprTableColumn tn cn
    _                                               -> throwError "Semantic error"


-- code generator for between-expr
exprBetween :: Expr -> Expr -> Expr -> CodeGenEnv
exprBetween a b c = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr a >> dup                              -- stack: a,a
    >> cExpr c >> appendInst opJGt 0 labelAva ""   -- stack: a,a,c -> a
    >> dup                                         -- stack: a,a
    >> cExpr b >> appendInst opJLt 0 labelAva ""   -- stack: a,a,b -> a
    >> pop 1 >> putTrue
    >> getLabel >>= goto
    >> mkLabel labelAva
    >> pop 1 >> putFalse
    >> mkCurrentLabel


-- code generator for in-expr
exprIn :: Expr -> ValueList -> CodeGenEnv
exprIn a (ValueList vl) =
    let genValueList = do
            oldRes <- getRes
            insSet <- (>>) (putRes []) $ foldr ((>>) . \x
                        -> throwErrorIfNotConst x
                        >> cExpr x
                        >> getSet >>= \sn
                        -> appendInst opSetInsert sn 0 "") getRes vl
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
exprColumn cn = getMetadata >>= \mds -> case columnIdx cn mds of
    (-1,  0) -> throwError $ "Ambiguous column name: " ++ cn
    (-1, -1) -> throwError $ "No such column: " ++ cn
    (i ,  j) -> appendInst opColumn i j ""


-- code generator for table-column
exprTableColumn :: String -> String -> CodeGenEnv
exprTableColumn tn cn = getMetadata >>= \mds -> case tableColumnIdx tn cn mds of
    (-1, _) -> throwError $ "No such column: " ++ tn ++ "." ++ cn
    (i , j) -> appendInst opColumn i j ""


-- code generator for function-call-expr
exprFuncCall :: String -> [Expr] -> CodeGenEnv
exprFuncCall fn pl =
    let plLength   = length pl
        pl'        = map cExpr pl
     in getFuncDef >>= \fnDef -> case dropWhile ((fn/=) . fst3) fnDef of
             []  -> throwError $ "No such function: " ++ fn
             (_, args, Just op):_
                 | plLength < args -> throwError $ "Too few arguments to function: "  ++ fn
                 | plLength > args -> throwError $ "Too many arguments to function: " ++ fn
                 | otherwise -> foldr (>>) getRes pl'
                             >> appendInst op 0 0 ""
             (_, _, Nothing):_ -> throwError "Not implemented"


-- code generator for const-expr
exprConst :: Value -> CodeGenEnv
exprConst val = case val of
    ValStr str       -> appendInst opString  0   0 str
    ValInt int       -> appendInst opInteger int 0 ""
    ValDouble double -> appendInst opString  0   0 $ show double
    Null             -> appendInst opNull    0   0 ""


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
exprAnd e1 e2 = getLabel >>= \labelAva -> updateLabel
    >> cExpr e1
    >> appendInst opJIf 1 labelAva ""
    >> cExpr e2
    >> appendInst opJIf 1 labelAva ""
    >> putTrue  >> getLabel >>= goto
    >> mkLabel labelAva
    >> putFalse
    >> mkCurrentLabel


-- code generator for or-expr
exprOr :: Expr -> Expr -> CodeGenEnv
exprOr e1 e2 = getLabel >>= \labelAva -> updateLabel
    >> cExpr e1
    >> appendInst opJIf 0 labelAva ""
    >> cExpr e2
    >> appendInst opJIf 0 labelAva ""
    >> putFalse >> getLabel >>= goto
    >> mkLabel labelAva
    >> putTrue
    >> mkCurrentLabel


-- code generator for like/glob operators
cLikeOp :: LikeOp -> CodeGenEnv
cLikeOp op = case op of
    Like    -> appendInst opSetLike 0 1 ""
    Glob    -> appendInst opSetGlob 0 1 ""
    NotLike -> appendInst opSetLike 1 1 ""
    NotGlob -> appendInst opSetGlob 1 1 ""


-- code generator for arithmetic operators
cArithOp :: BinOp -> CodeGenEnv
cArithOp op = case op of
    Plus     -> appendInst opAdd      0 0 ""
    Minus    -> appendInst opSubtract 0 0 ""
    Multiply -> appendInst opMultiply 0 0 ""
    Divide   -> appendInst opDivide   0 0 ""
    _        -> throwError "Wrong op type"


-- code generator for comparison operators
cComprOp :: BinOp -> CodeGenEnv
cComprOp op =
    let chart = [
            (Ls, opSetLt), (LE, opSetLe), (Gt, opSetGt), (GE, opSetGe), (Eq, opSetEq), (NE, opSetNe)]
     in (\x -> appendInst x 0 1 "") . snd . head . dropWhile ((op /=) . fst) $ chart


----------------------------------------------------------
-- help functions
----------------------------------------------------------
-- make goto instruction
goto :: Int -> CodeGenEnv
goto label = appendInst opGoto 0 label ""

dup :: CodeGenEnv
dup = appendInst opDup 0 0 ""

pop :: Int -> CodeGenEnv
pop cnt = appendInst opPop cnt 0 ""

-- put true/false to stack
putTrue :: CodeGenEnv
putTrue = appendInst opInteger 1 0 ""

putFalse :: CodeGenEnv
putFalse = appendInst opInteger 0 0 ""

mkBoolVal :: OpCode -> Int -> Int -> String -> CodeGenEnv
mkBoolVal opCode p1 p2 p3 = appendInst opCode p1 p2 p3
    >> putFalse
    >> goto (p2 + 1)
    >> mkCurrentLabel
    >> putTrue
    >> mkCurrentLabel

-- const expr checker
isConstExpr :: Expr -> Bool
isConstExpr expr =
    case expr of
        BinExpr  _ a b         -> isConstExpr a && isConstExpr b
        LikeExpr _ a b         -> isConstExpr a && isConstExpr b
        ConstValue _           -> True
        FunctionCall _ a       -> foldr ((&&) . isConstExpr) True a
        IsNull a               -> isConstExpr a
        Between a b c          -> isConstExpr a && isConstExpr b && isConstExpr c
        InExpr a (ValueList b) -> foldr ((&&) . isConstExpr) (isConstExpr a) b
        NotExpr a              -> isConstExpr a
        _                      -> False

throwErrorIfNotConst :: Expr -> CodeGenEnv
throwErrorIfNotConst expr
    | isConstExpr expr = return []
    | otherwise        = throwError "Right-hand side of IN operator must be constant"