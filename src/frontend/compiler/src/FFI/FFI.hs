{-# LANGUAGE LambdaCase #-}

module FFI (
    getTableMetadata
) where


import FFIStructure

import Foreign.C
import Foreign.Ptr
import Foreign.Storable


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
    fromTableMetadata_c tableName metadata_c
