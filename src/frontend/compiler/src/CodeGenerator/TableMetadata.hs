{-# LANGUAGE LambdaCase #-}
module TableMetadata where


----------------------------------------------------------
-- converted structures
----------------------------------------------------------
type TableIndex = (String, [String])            -- (index-name, [column-name])

data TableMetadata = TableMetadata {
    metadata_name        :: String,             -- table-name
    metadata_index       :: [TableIndex],
    metadata_column      :: [String],
    metadata_cookie      :: Int
} deriving (Eq, Show)
