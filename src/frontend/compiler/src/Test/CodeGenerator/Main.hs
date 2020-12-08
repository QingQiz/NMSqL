module Main where


import TestCodeGenerator

import Test.HUnit
import Control.Monad


main :: IO ()
main = void (runTestTT codeGeneratorTest)
