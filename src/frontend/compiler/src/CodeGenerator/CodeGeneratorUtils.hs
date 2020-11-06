module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenState  = ([TableMetadata], [Instruction], Int)

type CodeGenEnv    = ExceptT String (State CodeGenState) [Instruction]

data ExprResultType = TBool | TAsIs


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
retRes :: CodeGenEnv
retRes = (\(_, b, _) -> b) <$> lift get

-- functions to operate label
getLabel    = (\(_, _, c) -> c) <$> lift get
putLabel l  = get >>= (\(a, b, _) -> put (a, b, l)) >> retRes
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

appendInst :: Instruction -> CodeGenEnv
appendInst inst = get >>= (\(a, b, c) -> put (a, b ++ [inst], c)) >> retRes

fst3 (a, _, _) = a
snd3 (_, a, _) = a
trd3 (_, _, a) = a
