{-# LANGUAGE LambdaCase #-}
module Expr (cExpr) where


import Ast
import Instruction
import CodeGeneratorUtils

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
            | op == In                                  -> throwError "not implemented"
            | op `elem` [Plus, Minus, Divide, Multiple] -> exprArith op e1 e2
            | otherwise                                 -> exprCompr op e1 e2
        LikeExpr op e1 e2                               -> exprLike  op e1 e2
        ConstValue val                                  -> exprConst val
        _ -> throwError $ "expression `" ++ show expr ++ "` is not supported."
    retRes


-- code generator for const-expr
exprConst val = case val of
    ValStr str       -> appendInst $ Instruction opString  0   0 str
    ValInt int       -> appendInst $ Instruction opInteger int 0 ""
    ValDouble double -> appendInst $ Instruction opString  0   0 $ show double


-- code generator for like-expr
exprLike op e1 e2  = cExpr e1 >> cExpr e2 >> cLikeOp  op


-- code generator for arith-expr
exprArith op e1 e2 = cExpr e1 >> cExpr e2 >> cArithOp op


-- code generator for compare-expr
exprCompr op e1 e2 = cExpr e1 >> cExpr e2 >> cComprOp op


-- code generator for and-expr
exprAnd e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 >> gotoWhenFalse labelAva
    >> cExpr e2 >> gotoWhenFalse labelAva
    >> putTrue        >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putFalse
    >> getLabel >>= mkLabel


-- code generator for or-expr
exprOr e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 >> gotoWhenTrue labelAva
    >> cExpr e2 >> gotoWhenTrue labelAva
    >> putFalse       >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putTrue
    >> getLabel >>= mkLabel


-- code generator for like/glob operators
cLikeOp op = case op of
    Like    -> trueAndMkRes  opLike
    Glob    -> trueAndMkRes  opGlob
    NotLike -> falseAndMkRes opLike
    NotGlob -> falseAndMkRes opGlob

-- code generator for arithmetic operators
cArithOp op = case op of
    Plus     -> appendInst $ Instruction opAdd      0 0 ""
    Minus    -> appendInst $ Instruction opSubtract 0 0 ""
    Multiple -> appendInst $ Instruction opMultiply 0 0 ""
    Divide   -> appendInst $ Instruction opDivide   0 0 ""


-- code generator for comparison operators
cComprOp op =
    let chart = [
            (Ls, opLt), (LE, opLe), (Gr, opGt), (GE, opGe),
            (Eq, opEq), (NE, opNe)]
     in trueAndMkRes . snd . head . dropWhile ((op /=) . fst) $ chart


----------------------------------------------------------
-- help functions
----------------------------------------------------------
-- make goto instruction
goto          label = appendInst $ Instruction opGoto 0 label ""
gotoWhenTrue  label = appendInst $ Instruction opIf   0 label ""
gotoWhenFalse label = appendInst  (Instruction opNot  0 0     "") >> gotoWhenTrue label


-- make label
mkLabelWithoutUpdate label = appendInst (Instruction opNoop 0 label "")
mkLabel label  = mkLabelWithoutUpdate label >> updateLabel
mkCurrentLabel = getLabel >>= mkLabel


-- put true/false to stack
putTrue  = appendInst $ Instruction opInteger 1 0 ""
putFalse = appendInst $ Instruction opInteger 0 0 ""


-- put result to stack after insturction is executed
-- opCode then goto true or fallback to false
mkBoolVal opCode p1 p2 p3 = appendInst (Instruction opCode p1 p2 p3)
    >> putFalse
    >> goto (p2 + 1)
    >> mkCurrentLabel
    >> putTrue
    >> mkCurrentLabel

trueAndMkRes  opCode = getLabel >>= \l -> mkBoolVal opCode 0 l ""
falseAndMkRes opCode = getLabel >>= \l -> mkBoolVal opCode 1 l ""
