module Main where


import TestCodeGen

import Test.HUnit


main :: IO ()
main = do
    runTestTT cgTests
    return ()
