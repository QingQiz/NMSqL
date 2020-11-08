module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenState = ([TableMetadata], [Instruction], Int)

type CodeGenEnv   = ExceptT String (State CodeGenState) [Instruction]

type CodeGenRes   = Either String [Instruction]


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
retRes :: CodeGenEnv
retRes = snd3 <$> lift get

-- functions to operate label
getLabel    = trd3 <$> lift get
putLabel l  = get >>= (\(a, b, _) -> put (a, b, l)) >> retRes
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

-- append an instruction to env
appendInst :: Instruction -> CodeGenEnv
appendInst inst = get >>= (\(a, b, c) -> put (a, b ++ [inst], c)) >> retRes

-- fst, snd, trd for (,,)
fst3 (a, _, _) = a
snd3 (_, a, _) = a
trd3 (_, _, a) = a

-- get table metadata from env
getMetadata :: ExceptT String (State CodeGenState) [TableMetadata]
getMetadata = fst3 <$> lift get
