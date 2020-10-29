{-# LANGUAGE LambdaCase #-}

module FFI (
    TableMetadata,
    getTableMetadata
) where


import Data.Int  (Int32)
import Data.List (unfoldr)
-- provide foreign data type
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


----------------------------------------------------------
-- foreign language structures
----------------------------------------------------------
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
    poke ptr (TableMetadata_c ic i cc c cookie) = do
        pokeByteOff ptr 0  ic
        pokeByteOff ptr 8  i
        pokeByteOff ptr 16 cc
        pokeByteOff ptr 23 c
        pokeByteOff ptr 32 cookie


----------------------------------------------------------
-- converted structures
----------------------------------------------------------
type TableIndex = (String, [String])

data TableMetadata = TableMetadata {
    metadata_index       :: [TableIndex],
    metadata_column      :: [String],
    metadata_cookie      :: Int
}

fromTableMetadata_c :: TableMetadata_c -> IO TableMetadata
fromTableMetadata_c metadata_c =
    let tableIndex  = map parseIndex <$> peekStringArray(fromIntegral $ c_metadata_index_cnt  metadata_c) (c_metadata_index metadata_c)
        tableColumn = peekStringArray (fromIntegral $ c_metadata_column_cnt metadata_c) (c_metadata_column metadata_c)
        tableCookie = return (fromIntegral $ c_metadata_cookie metadata_c)
     in TableMetadata <$> tableIndex <*> tableColumn <*> tableCookie
    where
        -- inedx-name:column-name[,column-name]*
        parseIndex = fmap (unfoldr (\case {[] -> Nothing; l -> Just . fmap (drop 1). break (==',') $ l})) . fmap (drop 1) . break (==':')


----------------------------------------------------------
-- foreign functions to import
----------------------------------------------------------
foreign import ccall "getTableMetadata" getTableMetadata_c :: CString -> Ptr TableMetadata_c


----------------------------------------------------------
-- functions to export to foreign
----------------------------------------------------------
-- foreign export ccall compile :: CString -> CString


----------------------------------------------------------
-- wrappered foreign functions
----------------------------------------------------------
getTableMetadata :: String -> IO TableMetadata
getTableMetadata tableName = do
    tbName <- newCString tableName
    metadata_c <- peek $ getTableMetadata_c tbName
    fromTableMetadata_c metadata_c


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
peekStringArray :: Int -> Ptr CString -> IO [String]
peekStringArray size ptr = peekArray size ptr >>= foldr (\x z -> (:) <$> peekCString x <*> z) (return [])
