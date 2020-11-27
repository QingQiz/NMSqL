module CodeGeneratorUtils where


import Instruction
import FFIStructure

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
type CodeGenCnt   = (Int, Int, Int) -- (label-cnt, set-cnt, AggCnt)

type FunctionDef  = (String, Int) -- (func-name, func-param-cnt)

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


-- append an instruction to env
appendInst :: OpCode -> Int -> Int -> String -> CodeGenEnv
appendInst opCode p1 p2 p3 = get >>= (\(a, b, c) -> put (a, b ++ [Instruction opCode p1 p2 p3], c)) >> getRes

-- prepend the result of env to current env
prependEnv :: CodeGenEnv -> CodeGenEnv
prependEnv env = do
    oldRes <- getRes
    newRes <- putRes [] >> env
    putRes $ newRes ++ oldRes


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