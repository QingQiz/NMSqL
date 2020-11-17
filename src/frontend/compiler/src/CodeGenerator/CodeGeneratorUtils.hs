module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenCnt   = (Int, Int, Int) -- (label-cnt, set-cnt, cursor-cnt)

type FunctionDef  = (String, Int, Maybe OpCode)

type CodeGenState = (([TableMetadata], [FunctionDef]), [Instruction], CodeGenCnt)

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
getLabel = fst3 . trd3 <$> lift get

putLabel :: Int -> CodeGenEnv
putLabel l = get >>= (\(a, b, (_, d, e)) -> put (a, b, (l, d, e))) >> getRes

updateLabel :: CodeGenEnv
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)


-- functions to operate set
getSet :: ExceptTEnv Int
getSet = snd3 . trd3 <$> lift get

putSet :: Int -> CodeGenEnv
putSet s = get >>= (\(a, b, (c, _, e)) -> put (a, b, (c, s, e))) >> getRes

updateSet :: CodeGenEnv
updateSet = getSet >>= \x -> putSet $ x + 1


-- functions to operate cursor
getCursor :: ExceptTEnv Int
getCursor = trd3 . trd3 <$> lift get

putCursor :: Int -> CodeGenEnv
putCursor x = get >>= (\(a, b, (c, d, _)) -> put (a, b, (c, d, x))) >> getRes

updateCursor :: CodeGenEnv
updateCursor = getCursor >>= \x -> putCursor $ x + 1


-- append an instruction to env
appendInst :: OpCode -> Int -> Int -> String -> CodeGenEnv
appendInst opCode p1 p2 p3 = get >>= (\(a, b, c) -> put (a, b ++ [Instruction opCode p1 p2 p3], c)) >> getRes

-- fst, snd, trd for (,,)
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

trd3 :: (a, b, c) -> c
trd3 (_, _, a) = a

-- get table metadata from env
getMetadata :: ExceptTEnv [TableMetadata]
getMetadata = fst . fst3 <$> lift get

getFuncDef :: ExceptTEnv [FunctionDef]
getFuncDef = snd . fst3 <$> lift get
