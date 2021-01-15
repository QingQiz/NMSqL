module Main where


import TestParser
import TestCodeGenerator

import Test.HUnit
import Control.Monad


main :: IO ()
main = runTestTT codeGeneratorTest >> runTestTT parserTest >> return ()
