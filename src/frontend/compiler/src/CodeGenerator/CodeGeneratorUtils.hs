module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenCnt   = (Int, Int) -- (label-cnt, set-cnt)

type CodeGenState = ([TableMetadata], [Instruction], CodeGenCnt)

type ExceptTEnv a = ExceptT String (State CodeGenState) a

type CodeGenEnv   = ExceptTEnv [Instruction]

type CodeGenRes   = Either String [Instruction]


----------------------------------------------------------
-- some help functions
----------------------------------------------------------
getRes :: CodeGenEnv
getRes = snd3 <$> lift get

putRes :: [Instruction] -> CodeGenEnv
putRes is = get >>= (\(a, _, c) -> put (a, is, c)) >> getRes

clrRes :: CodeGenEnv
clrRes = putRes []


-- functions to operate label
getLabel :: ExceptTEnv Int
getLabel = fst . trd3 <$> lift get

putLabel :: Int -> CodeGenEnv
putLabel l = get >>= (\(a, b, (_, d)) -> put (a, b, (l, d))) >> getRes

updateLabel :: CodeGenEnv
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)


-- functions to operate set
getSet :: ExceptTEnv Int
getSet = snd . trd3 <$> lift get

putSet :: Int -> CodeGenEnv
putSet s = get >>= (\(a, b, (c, _)) -> put (a, b, (c, s))) >> getRes

updateSet :: CodeGenEnv
updateSet = getSet >>= \x -> putSet $ x + 1

-- append an instruction to env
appendInst :: Instruction -> CodeGenEnv
appendInst inst = get >>= (\(a, b, c) -> put (a, b ++ [inst], c)) >> getRes

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
