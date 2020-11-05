module Expr (cExpr) where


import Ast
import Instruction
import CodeGeneratorUtils

import Control.Monad.Except


----------------------------------------------------------
-- Code Generator
----------------------------------------------------------
cExpr :: Expr -> ExprResultType -> CodeGenEnv
cExpr expr rtype = do
    case expr of
        BinExpr op e1 e2
            | op == And                                 -> exprAnd e1 e2
            | op == Or                                  -> exprOr  e1 e2
            | op == In                                  -> throwError "not implemented"
            | op `elem` [Plus, Minus, Divide, Multiple] -> exprArith op e1 e2
            | otherwise                                 -> exprCompr op e1 e2
        _ -> throwError $ "expression `" ++ show expr ++ "` is not supported."
    retRes


-- code generaotr for arith-expr
exprArith op e1 e2 = cExpr e1 TAsIs >> cExpr e2 TAsIs >> cArithOp op 


-- code generaotr for compare-expr
exprCompr op e1 e2 = cExpr e1 TAsIs >> cExpr e2 TAsIs >> cComprOp op


-- code generator for and-expr
exprAnd e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 TBool >> gotoWhenFalse labelAva
    >> cExpr e2 TBool >> gotoWhenFalse labelAva
    >> putTrue        >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putFalse
    >> getLabel >>= mkLabel


-- code generator for or-expr
exprOr e1 e2 = getLabel >>= \labelAva
    -> updateLabel
    >> cExpr e1 TBool >> gotoWhenTrue labelAva
    >> cExpr e2 TBool >> gotoWhenTrue labelAva
    >> putFalse       >> getLabel >>= goto
    >> mkLabelWithoutUpdate labelAva
    >> putTrue
    >> getLabel >>= mkLabel


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
     where
        trueAndMkRes opCode = getLabel
            >>= (\x -> appendInst $ Instruction opCode 0 x "")
            >> putFalse
            >> getLabel >>= (\x -> goto $ x + 1)
            >> mkCurrentLabel
            >> putTrue
            >> mkCurrentLabel


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

