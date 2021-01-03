module Main where

import FFI
import TableMetadata

import Test.HUnit


main :: IO ()
main = do
    md <- getTableMetadata "xxx"

    assertEqual "FFI import: getTableMetadata" md
        $ TableMetadata "xxx" [("idx_xxx_a_b", ["a", "b"]), ("idx_xxx_a", ["a"])] ["a", "b", "c"] 281

    allTbNames <- getAllTableNames

    assertEqual "FFI import: getAllTableNames" allTbNames
        $ ["xxx"]

