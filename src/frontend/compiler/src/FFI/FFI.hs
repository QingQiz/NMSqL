{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI (
    getTableMetadata,
    getAllTableNames
) where


import Ast
import Instruction
import TableMetadata
import CodeGenerator
import CodeGeneratorUtils
import qualified Parser as P


import Data.Int  (Int32)
import Data.List

-- provide foreign data type
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Control.Monad.Except
import Control.Monad.State


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
        mkEnv :: IO CodeGenState
        mkEnv = do
            tbNames <- getAllTableNames
            mds <- mapM getTableMetadata tbNames
            return $ (,,)
                ((,)
                    (mds, mds)
                    [("max", 2), ("min", 2), ("substr", 3)])
                ([], [], 0)
                (CodeGenCnt 0 0 0 0 [] False)

        runCodeGeneratorWrapper :: SQL -> IO String
        runCodeGeneratorWrapper = \case
            SQLSelect  select -> runCodeGenerator (cSelectWrapper select Normal) <$> mkEnv
            SQLIndex indexAct -> runCodeGenerator (cIndexActionWrapper indexAct) <$> mkEnv
            SQLTable tableAct -> runCodeGenerator (cTableActionWrapper tableAct) <$> mkEnv
            SQLDelete delStmt -> runCodeGenerator (cDeleteWrapper       delStmt) <$> mkEnv
            SQLInsert insStmt -> runCodeGenerator (cInsertWrapper       insStmt) <$> mkEnv
            SQLUpdate updStmt -> runCodeGenerator (cUpdateWrapper       updStmt) <$> mkEnv

        runCodeGenerator :: CodeGenEnv -> CodeGenState -> String
        runCodeGenerator codeGenerator codeGenState =
            case evalState (runExceptT (codeGenerator >> fixJmp)) codeGenState of
                Right res -> show' res
                Left  msg -> err msg


----------------------------------------------------------
-- foreign functions to import
----------------------------------------------------------
foreign import ccall "getTableMetadata" getTableMetadata_c :: CString -> Ptr TableMetadata_c
foreign import ccall "getAllTableNames" getAllTableNames_c :: IO (Ptr CString)


----------------------------------------------------------
-- wrappered foreign functions
----------------------------------------------------------
getTableMetadata :: String -> IO TableMetadata
getTableMetadata tableName = do
    tbName <- newCString tableName
    metadata_c <- peek $ getTableMetadata_c tbName
    fromTableMetadata_c tableName metadata_c


getAllTableNames :: IO [String]
getAllTableNames = do
    res_c <- getAllTableNames_c >>= \tbNames_ptr -> peekArray0 nullPtr tbNames_ptr
    mapM peekCString res_c


fromTableMetadata_c :: String -> TableMetadata_c -> IO TableMetadata
fromTableMetadata_c tn metadata_c =
    let tableIndex  = map parseIndex <$> peekStringArray (fromIntegral $ c_metadata_index_cnt  metadata_c) (c_metadata_index metadata_c)
        tableColumn = peekStringArray (fromIntegral $ c_metadata_column_cnt metadata_c) (c_metadata_column metadata_c)
        tableCookie = return (fromIntegral $ c_metadata_cookie metadata_c)
     in TableMetadata tn <$> tableIndex <*> tableColumn <*> tableCookie
    where
        -- inedx-name:column-name[,column-name]*
        parseIndex = fmap (unfoldr (\case {[] -> Nothing; l -> Just . fmap (drop 1). break (==',') $ l}) . drop 1) . break (==':')


----------------------------------------------------------
-- foreign language structures
----------------------------------------------------------
{- HLINT ignore "Use camelCase" -}
data TableMetadata_c = TableMetadata_c {
    c_metadata_index_cnt  :: Int32,
    c_metadata_index      :: Ptr CString,
    c_metadata_column_cnt :: Int32,
    c_metadata_column     :: Ptr CString,
    c_metadata_cookie     :: Int32
}

instance Storable TableMetadata_c where
    alignment _ = 8
    sizeOf    _ = 40
    peek ptr    = TableMetadata_c
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 24
        <*> peekByteOff ptr 32
    poke ptr (TableMetadata_c ic i cc c cookie') = do
        pokeByteOff ptr 0  ic
        pokeByteOff ptr 8  i
        pokeByteOff ptr 16 cc
        pokeByteOff ptr 23 c
        pokeByteOff ptr 32 cookie'


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

peekStringArray :: Int -> Ptr CString -> IO [String]
peekStringArray size ptr = peekArray size ptr >>= foldr (\x z -> (:) <$> peekCString x <*> z) (return [])
