{-# LANGUAGE LambdaCase #-}

module CodeGenerator where


import Ast
import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- instance of code generator
----------------------------------------------------------
type CodeGenState  = ([TableMetadata], [Instruction], Int)

type CodeGenEnv    = ExceptT String (State CodeGenState) [Instruction]

data ExprResultType = TBool | TAsIs


cExpr :: Expr -> ExprResultType -> CodeGenEnv
cExpr expr otype = do
    (metadata, instructions, labelAva) <- lift get
    case expr of
        BinExpr op e1 e2
            | op == And
                -> updateLabel
                >> cExpr e1 TBool >> gotoWhenFalse labelAva
                >> cExpr e2 TBool >> gotoWhenFalse labelAva
                >> putTrue        >> getLabel >>= goto
                >> mkLabelWithoutUpdate labelAva
                >> putFalse
                >> getLabel >>= mkLabel
            | op == Or
                -> updateLabel
                >> cExpr e1 TBool >> gotoWhenTrue labelAva
                >> cExpr e2 TBool >> gotoWhenTrue labelAva
                >> putFalse       >> getLabel >>= goto
                >> mkLabelWithoutUpdate labelAva
                >> putTrue
                >> getLabel >>= mkLabel
        _ -> throwError $ "expression `" ++ show expr ++ "` is not supported."
    retRes
    where
        goto          label = appendInst $ Instruction opGoto 0 label ""
        gotoWhenTrue  label = appendInst $ Instruction opIf 0 label ""

        gotoWhenFalse label = appendInst (Instruction opNot 0 0 "") >> gotoWhenTrue label

        mkLabelWithoutUpdate label = appendInst (Instruction opNoop 0 label "")
        mkLabel label =  mkLabelWithoutUpdate label >> updateLabel

        putTrue  = appendInst $ Instruction opInteger 1 0 ""
        putFalse = appendInst $ Instruction opInteger 0 0 ""



----------------------------------------------------------
-- some help functions
----------------------------------------------------------
retRes :: CodeGenEnv
retRes = (\(_, b, _) -> b) <$> lift get

-- functions to operate label
getLabel    = (\(_, _, c) -> c) <$> lift get
putLabel l  = get >>= (\(a, b, _) -> put (a, b, l)) >> retRes
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

appendInst :: Instruction -> CodeGenEnv
appendInst inst = get >>= (\(a, b, c) -> put (a, b ++ [inst], c)) >> retRes
