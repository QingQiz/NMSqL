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
type CodeGenState = ([TableMetadata], [Instruction])

type CodeGenEnv   = ExceptT String (State CodeGenState) [Instruction]


cExpr :: Expr -> CodeGenEnv
cExpr expr = do
    (metadata, instructions) <- lift get
    case expr of
        BinExpr op e1 e2 -> cExpr e1 >> cExpr e2 >> cOp op >> get >>= put
        _ -> put (metadata, instructions ++ [Instruction opColumn 0 0 ""])
    snd <$> lift get


cOp :: BinOp -> CodeGenEnv
cOp op = do
    (metadata, instructions) <- lift get
    put (metadata, instructions ++ [Instruction opLt 0 0 ""])
    snd <$> lift get


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
notImplemented = error "Not Implemented Error."
