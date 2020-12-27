{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI (
    getTableMetadata
) where


import Ast
import Instruction
import TableMetadata
import CodeGenerator
import CodeGeneratorUtils
import qualified Parser as P

import Data.List

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Control.Monad.Except
import Control.Monad.State


----------------------------------------------------------
-- foreign functions to import
----------------------------------------------------------
foreign import ccall "getTableMetadata" getTableMetadata_c :: CString -> Ptr TableMetadata_c


----------------------------------------------------------
-- functions to export to foreign
----------------------------------------------------------
foreign export ccall compile :: CString -> IO CString

compile :: CString -> IO CString
compile sql_c = do
    sql <- peekCString sql_c
    case P.parse P.sql sql of
        Nothing       -> newCString $ err "parse error"
        Just (ast, _) -> newCString =<< runCodeGeneratorWrapper ast
    where
        mkEnv :: [String] -> IO CodeGenState
        mkEnv tables =
            let mds = mapM getTableMetadata tables
             in mds >>= \mds' -> return $ (,,)
                    ((,)
                        (mds', mds')
                        [("max", 2), ("min", 2), ("substr", 3)])
                    ([], [], 0)
                    (CodeGenCnt 0 0 0 0 [] False)

        runCodeGeneratorWrapper :: SQL -> IO String
        runCodeGeneratorWrapper = \case
            SQLSelect select -> runCodeGenerator (cSelectWrapper select Normal) <$> mkEnv (selectTableName select)
            _ -> return $ err "not implemented"

        runCodeGenerator :: CodeGenEnv -> CodeGenState -> String
        runCodeGenerator codeGenerator codeGenState =
            case evalState (runExceptT codeGenerator) codeGenState of
                Right res -> show' res
                Left  msg -> err msg

----------------------------------------------------------
-- wrappered foreign functions
----------------------------------------------------------
getTableMetadata :: String -> IO TableMetadata
getTableMetadata tableName = do
    tbName <- newCString tableName
    metadata_c <- peek $ getTableMetadata_c tbName
    fromTableMetadata_c tableName metadata_c


----------------------------------------------------------
-- help functions
----------------------------------------------------------
err :: String -> String
err msg = "SQL ERROR: " ++ msg

class Show' a where
    show' :: a -> String

instance Show' Instruction where
    show' (Instruction op p1 p2 p3) = show op ++ "|" ++ show p1 ++ "|" ++ show p2 ++ "|" ++ p3

instance Show' (Integer, Instruction) where
    show' (a, b) = show a ++ "|" ++ show' b

instance Show' [Instruction] where
    show' xs = intercalate "\n" $ zipWith (curry show') [0 :: Integer ..] xs