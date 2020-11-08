module Main where


import TestCodeGen

import Test.HUnit


main :: IO ()
main = runTestTT cgTests >> return ()
