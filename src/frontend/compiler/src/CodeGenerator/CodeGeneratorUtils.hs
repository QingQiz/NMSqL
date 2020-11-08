module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenState = ([TableMetadata], [Instruction], Int)

type ExceptTEnv a = ExceptT String (State CodeGenState) a

type CodeGenEnv   = ExceptTEnv [Instruction]

type CodeGenRes   = Either String [Instruction]


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
retRes :: CodeGenEnv
retRes = snd3 <$> lift get

-- functions to operate label
getLabel :: ExceptTEnv Int
getLabel    = trd3 <$> lift get

putLabel :: Int -> CodeGenEnv
putLabel l  = get >>= (\(a, b, _) -> put (a, b, l)) >> retRes

updateLabel :: CodeGenEnv
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

-- append an instruction to env
appendInst :: Instruction -> CodeGenEnv
appendInst inst = get >>= (\(a, b, c) -> put (a, b ++ [inst], c)) >> retRes

-- fst, snd, trd for (,,)
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

trd3 :: (a, b, c) -> c
trd3 (_, _, a) = a

-- get table metadata from env
getMetadata :: ExceptT String (State CodeGenState) [TableMetadata]
getMetadata = fst3 <$> lift get
