{-# LANGUAGE LambdaCase #-}
module CodeGeneratorUtils where


import Ast
import Instruction
import TableMetadata

import Data.List
import Data.Maybe
import Data.Bifunctor (first)
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except

import Debug.Trace


----------------------------------------------------------
-- some data structure
----------------------------------------------------------
data CodeGenCnt = CodeGenCnt {
    labelCNT     :: Int,
    setCNT       :: Int,
    aggCNT       :: Int,
    cursorCNT    :: Int,
    cursorOPENED :: [String],
    openWRITE    :: Bool
} -- (label-cnt, set-cnt, AggCnt)

type FunctionDef  = (String, Int) -- (func-name, func-param-cnt)

type CodeGenCache = ([Instruction], [Instruction], Int)

type CodeGenState = ((([TableMetadata], [TableMetadata]), [FunctionDef]), CodeGenCache, CodeGenCnt)

type ExceptTEnv a = ExceptT String (State CodeGenState) a

type CodeGenEnv   = ExceptTEnv [Instruction]

type CodeGenRes   = Either String [Instruction]

data SelectResultType = ToSet Int | ToSorter | Normal | UnionSel CompoundOp Int | ToTemp

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
getLabel = labelCNT . trd3 <$> lift get

putLabel :: Int -> CodeGenEnv
putLabel l = get >>= (\(a, b, cnt) -> put (a, b, cnt {labelCNT = l})) >> getRes

updateLabel :: CodeGenEnv
updateLabel = getLabel >>= (\x -> putLabel $ x + 1)

-- make label
mkLabel :: Int -> CodeGenEnv
mkLabel label = appendInst opNoop 0 label ""

mkCurrentLabel :: CodeGenEnv
mkCurrentLabel = getLabel >>= mkLabel >> updateLabel

-- functions to operate set
getSet :: ExceptTEnv Int
getSet = setCNT . trd3 <$> lift get

putSet :: Int -> CodeGenEnv
putSet s = get >>= (\(a, b, cnt) -> put (a, b, cnt {setCNT = s})) >> getRes

updateSet :: CodeGenEnv
updateSet = getSet >>= \x -> putSet $ x + 1

-- functions to operate agg
getAgg :: ExceptTEnv Int
getAgg = aggCNT . trd3 <$> lift get

putAgg :: Int -> CodeGenEnv
putAgg agg = get >>= (\(a, b, cnt) -> put (a, b, cnt {aggCNT = agg})) >> getRes

updateAgg :: CodeGenEnv
updateAgg = getAgg >>= \x -> putAgg $ x + 1

-- cursor
getCursor :: ExceptTEnv Int
getCursor = cursorCNT . trd3 <$> lift get

putCursor :: Int -> CodeGenEnv
putCursor x = get >>= (\(a, b, cnt) -> put (a, b, cnt {cursorCNT = x})) >> getRes

getCursorOpened :: ExceptTEnv [String]
getCursorOpened = cursorOPENED . trd3 <$> lift get

putCursorOpened :: [String] -> CodeGenEnv
putCursorOpened x = get >>= (\(a, b, cnt) -> put (a, b, cnt {cursorOPENED = x})) >> getRes

-- open flag
getWriteFlag :: ExceptTEnv Bool
getWriteFlag = openWRITE . trd3 <$> lift get

putWriteFlag :: Bool -> CodeGenEnv
putWriteFlag x = get >>= (\(a, b, cnt) -> put (a, b, cnt {openWRITE = x})) >> getRes

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

appendEnv :: CodeGenEnv -> CodeGenEnv
appendEnv env = do
    oldRes <- getRes
    newRes <- putRes [] >> env
    putRes $ oldRes ++ newRes

insertEnvWhere :: CodeGenEnv -> (Instruction -> Bool) -> CodeGenEnv
insertEnvWhere env cond = do
    oldRes <- break cond <$> getRes
    newRes <- putRes [] >> env >> getRes
    putRes (fst oldRes ++ newRes ++ snd oldRes)

insertEnvWhere2 :: CodeGenEnv -> (Instruction -> Instruction -> Bool) -> CodeGenEnv
insertEnvWhere2 env cond = do
    oldRes <- break' cond <$> getRes
    newRes <- putRes [] >> env >> getRes
    putRes (fst oldRes ++ newRes ++ snd oldRes)
    where
        break' _ []  = ([ ], [])
        break' _ [a] = ([a], [])
        break' x l   =
            let res = zipWith x l (tail l)
             in case elemIndex True res of
                     Nothing -> (l, [])
                     Just  i -> splitAt (i + 1) l

insertTemp :: CodeGenEnv -> CodeGenEnv
insertTemp = flip insertEnvWhere (==Instruction opTempInst 0 0 "")

filterEnv :: (Instruction -> Bool) -> CodeGenEnv
filterEnv cond = do
    res <- filter cond <$> getRes
    putRes res

removeTemp :: CodeGenEnv
removeTemp = filterEnv ((/=opTempInst) . iOpCode)

-- fst, snd, trd for (,,)
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

trd3 :: (a, b, c) -> c
trd3 (_, _, a) = a

-- get table metadata from env
getMetadata :: ExceptTEnv [TableMetadata]
getMetadata = fst . fst . fst3 <$> lift get

-- alter metadata
putMetadata :: [TableMetadata] -> CodeGenEnv
putMetadata md = get >>= (\(((_, md2), b), c, d)
    -> put (((md, md2), b), c, d)) >> doNothing

filterMetadata :: (TableMetadata -> Bool) -> CodeGenEnv
filterMetadata cond = get >>= (\(((_, md2), b), c, d)
    -> put (((filter cond md2, md2), b), c, d)) >> doNothing

resetMetadata :: CodeGenEnv
resetMetadata = get >>= (\(((_, md2), b), c, d)
    -> put (((md2, md2), b), c, d)) >> doNothing

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

-- random number generator, see `https://en.wikipedia.org/wiki/Linear_congruential_generator`
nextCookie :: Integral a => a -> a
nextCookie n = (7368787 * n + 1299709) `mod` 2147483647


tbExists :: String -> [TableMetadata] -> Bool
tbExists _ [] = False
tbExists tbName (md:mds) = (metadata_name md == tbName) || tbExists tbName mds


cookie :: [TableMetadata] -> Int
cookie = metadata_cookie . head

newCookie :: [TableMetadata] -> Int
newCookie = nextCookie . cookie

fixJmp :: CodeGenEnv
fixJmp = getRes >>= \res ->
    let res' = M.fromList $ map (first iP2) $ filter ((==opNoop) . iOpCode . fst) $ zip res [0::Int ..]
        insList = [opNext        ,opNextIdx     ,opJSetFound   ,opJIf         ,opJSetNotFound
                  ,opJLike       ,opJGlob       ,opJNe         ,opJEq         ,opJNe
                  ,opJLt         ,opJLe         ,opJGt         ,opJGe         ,opJIsNull
                  ,opJNotNull    ,opGoto        ]
        trans x = if iOpCode x `elem` insList
                  then Instruction (iOpCode x) (iP1 x) (fromMaybe (-1) $ M.lookup (iP2 x) res') (iP3 x)
                  else x
     in putRes (map trans res)
