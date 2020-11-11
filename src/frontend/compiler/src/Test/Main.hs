module Main where


import TestCodeGenerator

import Test.HUnit


main :: IO ()
main = runTestTT codeGeneratorTest >> return ()
