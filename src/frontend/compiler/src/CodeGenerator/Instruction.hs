{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Instruction where

opTempInst     = "opTempInst"      --0
opTransaction  = "opTransaction"   --1
opCommit       = "opCommit"        --2
opRollback     = "opRollback"      --3
opReadCookie   = "opReadCookie"    --4
opSetCookie    = "opSetCookie"     --5
opVerifyCookie = "opVerifyCookie"  --6
opOpen         = "opOpen"          --7
opOpenTemp     = "opOpenTemp"      --8
opOpenWrite    = "opOpenWrite"     --9
opClose        = "opClose"         --10
opMoveTo       = "opMoveTo"        --11
opFcnt         = "opFcnt"          --12
opNewRecno     = "opNewRecno"      --13
opPut          = "opPut"           --14
opDistinct     = "opDistinct"      --15
opFound        = "opFound"         --16
opNotFound     = "opNotFound"      --17
opDelete       = "opDelete"        --18
opColumn       = "opColumn"        --19
opKeyAsData    = "opKeyAsData"     --20
opRecno        = "opRecno"         --21
opFullKey      = "opFullKey"       --22
opRewind       = "opRewind"        --23
opNext         = "opNext"          --24
opDestroy      = "opDestroy"       --25
opClear        = "opClear"         --26
opCreateIndex  = "opCreateIndex"   --27
opCreateTable  = "opCreateTable"   --28
opReorganize   = "opReorganize"    --29
opBeginIdx     = "opBeginIdx"      --30
opNextIdx      = "opNextIdx"       --31
opPutIdx       = "opPutIdx"        --32
opDeleteIdx    = "opDeleteIdx"     --33
opMemLoad      = "opMemLoad"       --34
opMemStore     = "opMemStore"      --35
opListOpen     = "opListOpen"      --36
opListWrite    = "opListWrite"     --37
opListRewind   = "opListRewind"    --38
opListRead     = "opListRead"      --39
opListClose    = "opListClose"     --40
opSortOpen     = "opSortOpen"      --41
opSortPut      = "opSortPut"       --42
opSortMakeRec  = "opSortMakeRec"   --43
opSortMakeKey  = "opSortMakeKey"   --44
opSort         = "opSort"          --45
opSortNext     = "opSortNext"      --46
opSortKey      = "opSortKey"       --47
opSortCallback = "opSortCallback"  --48
opSortClose    = "opSortClose"     --49
opFileOpen     = "opFileOpen"      --50
opFileRead     = "opFileRead"      --51
opFileColumn   = "opFileColumn"    --52
opFileClose    = "opFileClose"     --53
opAggReset     = "opAggReset"      --54
opAggFocus     = "opAggFocus"      --55
opAggIncr      = "opAggIncr"       --56
opAggNext      = "opAggNext"       --57
opAggSet       = "opAggSet"        --58
opAggGet       = "opAggGet"        --59
opSetInsert    = "opSetInsert"     --60
opJSetFound    = "opJSetFound"      --61
opJSetNotFound = "opJSetNotFound"   --62
opSetClear     = "opSetClear"      --63
opMakeRecord   = "opMakeRecord"    --64
opMakeKey      = "opMakeKey"       --65
opMakeIdxKey   = "opMakeIdxKey"    --66
opGoto         = "opGoto"          --67
opJIf          = "opJIf"           --68
opHalt         = "opHalt"          --69
opColumnCount  = "opColumnCount"   --70
opColumnName   = "opColumnName"    --71
opCallback     = "opCallback"      --72
opInteger      = "opInteger"       --73
opString       = "opString"        --74
opNull         = "opNull"          --75
opPop          = "opPop"           --76
opDup          = "opDup"           --77
opPull         = "opPull"          --78
opAdd          = "opAdd"           --79
opAddImm       = "opAddImm"        --80
opSubtract     = "opSubtract"      --81
opMultiply     = "opMultiply"      --82
opDivide       = "opDivide"        --83
opMin          = "opMin"           --84
opMax          = "opMax"           --85
opJLike        = "opJLike"         --86
opJGlob        = "opJGlob"         --87
opJEq          = "opJEq"           --88
opJNe          = "opJNe"           --89
opJLt          = "opJLt"           --90
opJLe          = "opJLe"           --91
opJGt          = "opJGt"           --92
opJGe          = "opJGe"           --93
opJIsNull      = "opJIsNull"       --94
opJNotNull     = "opJNotNull"      --95
opNegative     = "opNegative"      --96
opAnd          = "opAnd"           --97
opOr           = "opOr"            --98
opNot          = "opNot"           --99
opConcat       = "opConcat"        --100
opNoop         = "opNoop"          --101
opStrlen       = "opStrlen"        --102
opSubstr       = "opSubstr"        --103
opSetIf        = "opSetIf"         --104
opSetLike      = "opSetLike"       --105
opSetGlob      = "opSetGlob"       --106
opSetEq        = "opSetEq"         --107
opSetNe        = "opSetNe"         --108
opSetLt        = "opSetLt"         --109
opSetLe        = "opSetLe"         --110
opSetGt        = "opSetGt"         --111
opSetGe        = "opSetGe"         --112
opSetIsNull    = "opSetIsNull"     --113
opSetNotNull   = "opSetNotNull"    --114
opSortSetDesc  = "opSortSetDesc"   --115
opSetOpen      = "opSetOpen"       --116
opSetSetFound  = "opSetSetFound"   --117
opSetSetNotFound = "opSetSetNotFound"--118
opSetSetEmpty  = "opSetSetEmpty"  --119

type OpCode = String

data Instruction = Instruction {
    iOpCode :: OpCode,
    iP1     :: Int,
    iP2     :: Int,
    iP3     :: String
} deriving Eq


instance Show Instruction where
    show (Instruction op p1 p2 p3) =
        let op' = if length op < 8 then op ++ "\t" else op
         in "\n\t" ++ op' ++ "\t" ++ show p1 ++ "\t" ++ show p2 ++ "\t" ++ show p3
