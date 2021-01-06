module TestParser where


import Ast
import Parser
import TestUtils

import Test.HUnit


parserTest :: Test
parserTest = test [
----------------------------------------------------------
-- Test parser: value
----------------------------------------------------------
      "int value"   ~:  "int: 123"
                    ~:  runParser value "123"
                    ~?= ValInt 123

    , "int value"   ~:  "int: -123"
                    ~:  runParser value "-123"
                    ~?= ValInt (-123)
----------------------------------------------------------
    , "double value"~:  "double: 123."
                    ~:  runParser value "123."

                    ~?= ValDouble 123.0
    , "double value"~:  "double: 123.1"
                    ~:  runParser value "123.1"
                    ~?= ValDouble 123.1

    , "double value"~:  "double: - 123.1"
                    ~:  runParser value " - 123.1"
                    ~?= ValDouble (-123.1)
----------------------------------------------------------
-- Test parser: 
----------------------------------------------------------
    ]
