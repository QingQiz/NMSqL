{-# LANGUAGE LambdaCase #-}

module CodeGenerator where

import Ast
import FFI
import Instruction

import Control.Monad.State

----------------------------------------------------------
-- instance of code generator
----------------------------------------------------------
class CodeGenerator a where
    -- state is         : (Ast, TableMetadata)
    -- success return   : [Instruction]
    -- fail return      : String
    genCode :: StateT (a, TableMetadata) (Either String) [Instruction]


instance CodeGenerator Expr where
    genCode =
        let
            haveIndex = False
        in do
            (ast, metadata) <- get
            if haveIndex
            then notImplemented
            else notImplemented
        where



----------------------------------------------------------
-- some help functions
----------------------------------------------------------
notImplemented = error "Not Implemented Error."

