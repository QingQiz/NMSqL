{-# LANGUAGE LambdaCase #-}
module CodeGeneratorUtils where


import Ast
import Instruction
import FFIStructure

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenCnt   = (Int, Int, Int) -- (label-cnt, set-cnt, AggCnt)

type FunctionDef  = (String, Int) -- (func-name, func-param-cnt)

type CodeGenCache = ([Instruction], [Instruction], Int)

type CodeGenState = (([TableMetadata], [FunctionDef]), CodeGenCache, CodeGenCnt)

type ExceptTEnv a = ExceptT String (State CodeGenState) a

type CodeGenEnv   = ExceptTEnv [Instruction]

type CodeGenRes   = Either String [Instruction]

data SelectResultType = ToSet Int | ToSorter | Normal | UnionSel CompoundOp Int

----------------------------------------------------------
-- some help functions
----------------------------------------------------------
getRes :: CodeGenEnv
getRes = getCacheState >>= \case
    0 -> fst3 . snd3 <$> lift get
    _ -> snd3 . snd3 <$> lift get

putRes :: [Instruction] -> CodeGenEnv
putRes is = getCacheState >>= \case
    0 -> get >>= (\(a, (_, x1, x2), c) -> put (a, (is, x1, x2), c)) >> getRes
    _ -> get >>= (\(a, (x1, _, x2), c) -> put (a, (x1, is, x2), c)) >> getRes

clrRes :: CodeGenEnv
clrRes = putRes []

doNothing :: CodeGenEnv
doNothing = return []


-- functions to operate label
getLabel :: ExceptTEnv Int
getLabel = fst3 . trd3 <$> lift get

putLabel :: Int -> CodeGenEnv
putLabel l = get >>= (\(a, b, (_, d, e)) -> put (a, b, (l, d, e))) >> getRes

updateLabel :: CodeGenEnv
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

-- make label
mkLabel :: Int -> CodeGenEnv
mkLabel label = appendInst opNoop 0 label ""

mkCurrentLabel :: CodeGenEnv
mkCurrentLabel = getLabel >>= mkLabel >> updateLabel

-- functions to operate set
getSet :: ExceptTEnv Int
getSet = snd3 . trd3 <$> lift get

putSet :: Int -> CodeGenEnv
putSet s = get >>= (\(a, b, (c, _, d)) -> put (a, b, (c, s, d))) >> getRes

updateSet :: CodeGenEnv
updateSet = getSet >>= \x -> putSet $ x + 1

-- functions to operate agg
getAgg :: ExceptTEnv Int
getAgg = trd3 . trd3 <$> lift get

putAgg :: Int -> CodeGenEnv
putAgg agg = get >>= (\(a, b, (c, d, _)) -> put (a, b, (c, d, agg))) >> getRes

updateAgg :: CodeGenEnv
updateAgg = getAgg >>= \x -> putAgg $ x + 1

-- toggle cache
getCacheState :: ExceptTEnv Int
getCacheState = trd3 . snd3 <$> lift get

putCacheState :: Int -> CodeGenEnv
putCacheState cache = get >>= (\(a, (b, x1, _), c) -> put (a, (b, x1, cache), c)) >> getRes

applyCache :: CodeGenEnv
applyCache = get >>= (\(a, (b, x1, x2), c) -> put (a, (b ++ x1, [], x2), c)) >> getRes


-- append instructions to env
appendInstructions :: [Instruction] -> CodeGenEnv
appendInstructions insts = get >>= (\(a, (b, x1, x2), c) -> getCacheState >>= \case
    0 -> put (a, (b ++ insts, x1, x2), c) >> getRes
    _ -> put (a, (b, x1 ++ insts, x2), c) >> getRes)


-- append an instruction to env
appendInst :: OpCode -> Int -> Int -> String -> CodeGenEnv
appendInst opCode p1 p2 p3 = appendInstructions [Instruction opCode p1 p2 p3]


-- prepend the result of env to current env
prependEnv :: CodeGenEnv -> CodeGenEnv
prependEnv env = do
    oldRes <- getRes
    newRes <- putRes [] >> env
    putRes $ newRes ++ oldRes


-- insert env before tempInstruction
insertTemp :: CodeGenEnv -> CodeGenEnv
insertTemp env = do
    res <- break (==Instruction opTempInst 0 0 "") <$> getRes
    putRes (fst res) >> env >> appendInstructions (snd res)


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

-- put table metadata to env
putMetadata :: [TableMetadata] -> CodeGenEnv
putMetadata mds = get >>= (\((_, b), c, d) -> put ((mds, b), c, d)) >> doNothing

-- get function definations
getFuncDef :: ExceptTEnv [FunctionDef]
getFuncDef = snd . fst3 <$> lift get

putFuncDef :: [FunctionDef] -> CodeGenEnv
putFuncDef funcDef = get >>= (\((a, _), c, d) -> put ((a, funcDef), c, d)) >> getRes


-- get column index from metadatas, return: (table-index, column-index)
columnIdx :: String -> [TableMetadata] -> (Int, Int)
columnIdx cn mds =
    let valid     md = cn `elem` metadata_column md
        getColIdx md = fromMaybe (-1) $ elemIndex cn $ metadata_column md
     in case findIndices valid mds of
            [i] -> (i, getColIdx $ mds !! i)
            []  -> (-1, -1)  -- can not found
            _   -> (-1,  0)  -- can find column but can not determine which table to use


-- get table-column index from metadatas, return: (table-index, column-index)
tableColumnIdx :: String -> String -> [TableMetadata] -> (Int, Int)
tableColumnIdx tn cn mds = case findIndex ((==tn) . metadata_name) mds of
    Nothing -> (-1, 0)     -- no such table
    Just  i -> case elemIndex cn $ metadata_column $ mds !! i of
        Nothing -> (-1, 0) -- no such table
        Just  j -> (i, j)

-- connect CodeGenEnv
connectCodeGenEnv :: [CodeGenEnv] -> CodeGenEnv
connectCodeGenEnv = foldl (>>) (return [])


try :: CodeGenEnv -> CodeGenEnv -> CodeGenEnv
try a b = get >>= \st -> catchError a $ \_ -> put st >> b

-- debugger
dbgTrace :: Show a => a -> a
dbgTrace val = trace (show val) val

dbgShow :: Show a1 => a1 -> a2 -> a2
dbgShow val = trace (show val)

dbgShowRes :: CodeGenEnv
dbgShowRes = getRes >>= \res -> dbgShow res getRes