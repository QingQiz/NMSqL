{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Instruction where

opTempInst       = 0   :: Integer
opTransaction    = 1   :: Integer
opCommit         = 2   :: Integer
opRollback       = 3   :: Integer
opReadCookie     = 4   :: Integer
opSetCookie      = 5   :: Integer
opVerifyCookie   = 6   :: Integer
opOpen           = 7   :: Integer
opOpenTemp       = 8   :: Integer
opOpenWrite      = 9   :: Integer
opClose          = 10  :: Integer
opMoveTo         = 11  :: Integer
opFcnt           = 12  :: Integer
opNewRecno       = 13  :: Integer
opPut            = 14  :: Integer
opDistinct       = 15  :: Integer
opFound          = 16  :: Integer
opNotFound       = 17  :: Integer
opDelete         = 18  :: Integer
opColumn         = 19  :: Integer
opKeyAsData      = 20  :: Integer
opRecno          = 21  :: Integer
opFullKey        = 22  :: Integer
opRewind         = 23  :: Integer
opNext           = 24  :: Integer
opDestroy        = 25  :: Integer
opClear          = 26  :: Integer
opCreateIndex    = 27  :: Integer
opCreateTable    = 28  :: Integer
opReorganize     = 29  :: Integer
opBeginIdx       = 30  :: Integer
opNextIdx        = 31  :: Integer
opPutIdx         = 32  :: Integer
opDeleteIdx      = 33  :: Integer
opMemLoad        = 34  :: Integer
opMemStore       = 35  :: Integer
opListOpen       = 36  :: Integer
opListWrite      = 37  :: Integer
opListRewind     = 38  :: Integer
opListRead       = 39  :: Integer
opListClose      = 40  :: Integer
opSortOpen       = 41  :: Integer
opSortPut        = 42  :: Integer
opSortMakeRec    = 43  :: Integer
opSortMakeKey    = 44  :: Integer
opSort           = 45  :: Integer
opSortNext       = 46  :: Integer
opSortKey        = 47  :: Integer
opSortCallback   = 48  :: Integer
opSortClose      = 49  :: Integer
opFileOpen       = 50  :: Integer
opFileRead       = 51  :: Integer
opFileColumn     = 52  :: Integer
opFileClose      = 53  :: Integer
opAggReset       = 54  :: Integer
opAggFocus       = 55  :: Integer
opAggIncr        = 56  :: Integer
opAggNext        = 57  :: Integer
opAggSet         = 58  :: Integer
opAggGet         = 59  :: Integer
opSetInsert      = 60  :: Integer
opJSetFound      = 61  :: Integer
opJSetNotFound   = 62  :: Integer
opSetClear       = 63  :: Integer
opMakeRecord     = 64  :: Integer
opMakeKey        = 65  :: Integer
opMakeIdxKey     = 66  :: Integer
opGoto           = 67  :: Integer
opJIf            = 68  :: Integer
opHalt           = 69  :: Integer
opColumnCount    = 70  :: Integer
opColumnName     = 71  :: Integer
opCallback       = 72  :: Integer
opInteger        = 73  :: Integer
opString         = 74  :: Integer
opNull           = 75  :: Integer
opPop            = 76  :: Integer
opDup            = 77  :: Integer
opPull           = 78  :: Integer
opAdd            = 79  :: Integer
opAddImm         = 80  :: Integer
opSubtract       = 81  :: Integer
opMultiply       = 82  :: Integer
opDivide         = 83  :: Integer
opMin            = 84  :: Integer
opMax            = 85  :: Integer
opJLike          = 86  :: Integer
opJGlob          = 87  :: Integer
opJEq            = 88  :: Integer
opJNe            = 89  :: Integer
opJLt            = 90  :: Integer
opJLe            = 91  :: Integer
opJGt            = 92  :: Integer
opJGe            = 93  :: Integer
opJIsNull        = 94  :: Integer
opJNotNull       = 95  :: Integer
opNegative       = 96  :: Integer
opAnd            = 97  :: Integer
opOr             = 98  :: Integer
opNot            = 99  :: Integer
opConcat         = 100 :: Integer
opNoop           = 101 :: Integer
opStrlen         = 102 :: Integer
opSubstr         = 103 :: Integer
opSetIf          = 104 :: Integer
opSetLike        = 105 :: Integer
opSetGlob        = 106 :: Integer
opSetEq          = 107 :: Integer
opSetNe          = 108 :: Integer
opSetLt          = 109 :: Integer
opSetLe          = 110 :: Integer
opSetGt          = 111 :: Integer
opSetGe          = 112 :: Integer
opSetIsNull      = 113 :: Integer
opSetNotNull     = 114 :: Integer
opSortSetDesc    = 115 :: Integer
opSetOpen        = 116 :: Integer
opSetSetFound    = 117 :: Integer
opSetSetNotFound = 118 :: Integer
opSetSetEmpty    = 119 :: Integer
opDefaultKey     = 120 :: Integer
opAddress        = 121 :: Integer
opDouble         = 122 :: Integer

instructions = ["opTempInst"     ,"opTransaction" ,"opCommit"       ,"opRollback"    ,"opReadCookie"     ,"opSetCookie"
               ,"opVerifyCookie" ,"opOpen"        ,"opOpenTemp"     ,"opOpenWrite"   ,"opClose"          ,"opMoveTo"
               ,"opFcnt"         ,"opNewRecno"    ,"opPut"          ,"opDistinct"    ,"opFound"          ,"opNotFound"
               ,"opDelete"       ,"opColumn"      ,"opKeyAsData"    ,"opRecno"       ,"opFullKey"        ,"opRewind"
               ,"opNext"         ,"opDestroy"     ,"opClear"        ,"opCreateIndex" ,"opCreateTable"    ,"opReorganize"
               ,"opBeginIdx"     ,"opNextIdx"     ,"opPutIdx"       ,"opDeleteIdx"   ,"opMemLoad"        ,"opMemStore"
               ,"opListOpen"     ,"opListWrite"   ,"opListRewind"   ,"opListRead"    ,"opListClose"      ,"opSortOpen"
               ,"opSortPut"      ,"opSortMakeRec" ,"opSortMakeKey"  ,"opSort"        ,"opSortNext"       ,"opSortKey"
               ,"opSortCallback" ,"opSortClose"   ,"opFileOpen"     ,"opFileRead"    ,"opFileColumn"     ,"opFileClose"
               ,"opAggReset"     ,"opAggFocus"    ,"opAggIncr"      ,"opAggNext"     ,"opAggSet"         ,"opAggGet"
               ,"opSetInsert"    ,"opJSetFound"   ,"opJSetNotFound" ,"opSetClear"    ,"opMakeRecord"     ,"opMakeKey"
               ,"opMakeIdxKey"   ,"opGoto"        ,"opJIf"          ,"opHalt"        ,"opColumnCount"    ,"opColumnName"
               ,"opCallback"     ,"opInteger"     ,"opString"       ,"opNull"        ,"opPop"            ,"opDup"
               ,"opPull"         ,"opAdd"         ,"opAddImm"       ,"opSubtract"    ,"opMultiply"       ,"opDivide"
               ,"opMin"          ,"opMax"         ,"opJLike"        ,"opJGlob"       ,"opJEq"            ,"opJNe"
               ,"opJLt"          ,"opJLe"         ,"opJGt"          ,"opJGe"         ,"opJIsNull"        ,"opJNotNull"
               ,"opNegative"     ,"opAnd"         ,"opOr"           ,"opNot"         ,"opConcat"         ,"opNoop"
               ,"opStrlen"       ,"opSubstr"      ,"opSetIf"        ,"opSetLike"     ,"opSetGlob"        ,"opSetEq"
               ,"opSetNe"        ,"opSetLt"       ,"opSetLe"        ,"opSetGt"       ,"opSetGe"          ,"opSetIsNull"
               ,"opSetNotNull"   ,"opSortSetDesc" ,"opSetOpen"      ,"opSetSetFound" ,"opSetSetNotFound" ,"opSetSetEmpty"
               ,"opDefaultKey"   ,"opAddress"     ,"opDouble"]

type OpCode = Integer

data Instruction = Instruction {
    iOpCode :: OpCode,
    iP1     :: Int,
    iP2     :: Int,
    iP3     :: String
} deriving Eq


instance Show Instruction where
    show (Instruction op p1 p2 p3) =
        let op' = instructions !! fromIntegral op
            op'' = if length op' < 8 then op' ++ "\t" else op'
         in "\n\t" ++ op'' ++ "\t" ++ show p1 ++ "\t" ++ show p2 ++ "\t" ++ show p3
