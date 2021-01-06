{-# LANGUAGE LambdaCase #-}

module CodeGenerator where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils

import Generator.Expr (cExpr, isConstExpr)
import Generator.Select (cSelect)
import Generator.Table (cTableAction)
import Generator.Index (cIndexAction)
import Generator.Delete (cDelete)
import Generator.Insert (cInsert)
import Generator.Update (cUpdate)

import Data.List
import Data.Maybe
import qualified Data.Map as Map


cExprWrapper :: Expr -> CodeGenEnv
cExprWrapper expr = getMetadata >>= \mds -> uncurry (wrapperExpr mds) (splitExpr expr mds) where
    wrapperExpr :: [TableMetadata] ->  [(Int, String, [Expr])] -> [Expr] -> CodeGenEnv
    wrapperExpr mds inp condExpr = openTbAndIdx >> verifyCookie >> wrapper >> closeTbAndIdx where
        verifyCookie = appendInst opVerifyCookie (metadata_cookie (head mds)) 0 ""

        wrapper = getLabel >>= \lab -> updateLabel
                >> wrapperIdx idxNames (map trd3 inp) lab
                >> mkLabel lab

        len = length allTbNames

        openTbAndIdx =
            let listToOpen = collectTbOrIdx allTbNames tbNames idxNames
             in getCursor >>= \cr
             -> connectCodeGenEnv (zipWith(\name idx -> getWriteFlag >>= \wf
                                                     -> appendInst (if wf then opOpenWrite else opOpen) idx 0 name
                                                     >> getCursorOpened >>= \co
                                                     -> putCursorOpened (name : co)) listToOpen [cr .. len - 1 + cr])
            where
                collectTbOrIdx (tn:tns) tbs idxes
                    | tn `elem` tbs = head idxes : collectTbOrIdx tns (tail tbs) (tail idxes)
                    | otherwise     = tn         : collectTbOrIdx tns tbs idxes
                collectTbOrIdx _ _ _ = []

        closeTbAndIdx = getCursor >>= \cr
            -> connectCodeGenEnv [appendInst opClose i 0 "" | i <- [cr .. len - 1 + cr]]
            >> putCursor (cr + len)

        wrapperIdx :: [String] -> [[Expr]] -> Int -> CodeGenEnv
        wrapperIdx (idx:idxes) (key:keys) labEnd =
            let idxCursor = fromMaybe (-1)
                          -- find the index of the table who has index named idx
                          $ findIndex (elem idx . map fst . metadata_index) mds
                keyLength = length key
                mkKey = getCursor >>= \cr -> connectCodeGenEnv (map cExpr key)
                     >> appendInst opMakeKey  keyLength        0      ""
                     >> appendInst opBeginIdx (idxCursor + cr) labEnd ""
             in getCursor >>= \cr
             -> getLabel >>= \lab  -> updateLabel
             >> getLabel >>= \lab' -> updateLabel
             >> mkKey
             >> mkLabel lab
             >> wrapperIdx idxes keys lab'
             >> mkLabel lab'
             >> appendInstructions [Instruction opNextIdx (idxCursor + cr) labEnd ""
                                   ,Instruction opGoto    0                lab    ""]

        wrapperIdx _ _ labEnd = wrapperTb tbNotUseIdx labEnd where
            tbNotUseIdx = filter (`notElem` tbNames) allTbNames

            condExpr' lab = case condExpr of
                [] -> []
                _  -> [cExpr $ foldl1 (flip $ BinExpr And) condExpr
                      ,appendInst opJIf 1 lab ""]

            wrapperTb (tb:tbs) labE =
                let tbCursor = fromMaybe (-1)
                             $ findIndex (\md -> metadata_name md == tb) mds
                 in getLabel >>= \lab  -> updateLabel
                 >> getLabel >>= \lab' -> updateLabel
                 >> appendInst opRewind tbCursor labE ""
                 >> mkLabel lab
                 >> wrapperTb tbs lab'
                 >> mkLabel lab'
                 >> appendInst opNext tbCursor labE ""
                 >> appendInst opGoto 0 lab ""

            wrapperTb _ labE = connectCodeGenEnv (condExpr' labE)
                >> appendInst opTempInst 0 0    ""

        -- some help functions
        idxNames = map snd3 inp
        tbNames  = [metadata_name $ mds !! fst3 i | i <- inp]
        allTbNames  = map metadata_name mds

    splitExpr :: Expr -> [TableMetadata] -> ([(Int, String, [Expr])], [Expr])
    splitExpr e mds =
        let (eqPair, othExpr) = collectExpr e
            (key, combinedExpr) = splitEqPairToMkKey eqPair mds
         in (key, combinedExpr ++ othExpr)

    collectExpr :: Expr -> ([(Expr, Expr)], [Expr])
    collectExpr expr' = collectExpr' expr' [] [] where
        collectExpr' e@(BinExpr Eq e1 e2) a b =
            case (e1, e2) of
                (e1'@(Column _)       , e2') | isConstExpr e2' -> ((e1', e2') : a, b)
                (e1'@(TableColumn _ _), e2') | isConstExpr e2' -> ((e1', e2') : a, b)
                (e1', e2'@(Column _))        | isConstExpr e1' -> ((e2', e1') : a, b)
                (e1', e2'@(TableColumn _ _)) | isConstExpr e1' -> ((e2', e1') : a, b)
                _ -> (a, e : b)
        collectExpr' (BinExpr And e1 e2) a b =
            let (e1a, e1b) = collectExpr' e1 a b
             in collectExpr' e2 e1a e1b
        collectExpr' e a b = (a, e : b)

    groupEqPairByTable :: [(Expr, Expr)] -> [TableMetadata] -> Maybe [(Int, [(Expr, Expr)])]
    groupEqPairByTable eqPair mds = groupEqPairByTable' eqPair Map.empty where
        groupEqPairByTable' [] m = Just $ Map.toList m
        groupEqPairByTable' (p:ps) m = case p of
            (TableColumn tn cn, _) -> case tableColumnIdx tn cn mds of
                (-1, _) -> Nothing
                (i , _) -> groupEqPairByTable' ps $ updateMap i p m
            (Column cn, x) -> case columnIdx cn mds of
                (-1, _) -> Nothing
                (i , _) -> groupEqPairByTable' ps
                         $ updateMap i (TableColumn (metadata_name $ mds !! i) cn, x) m
            _ -> Nothing
            where
                updateMap tbIdx pair m' =
                    let oldVal = fromMaybe [] $ Map.lookup tbIdx m'
                     in Map.insert tbIdx (pair:oldVal) m'

    findBestIndexInColumnList :: [TableIndex] -> [Expr] -> Maybe TableIndex
    findBestIndexInColumnList tbIdxes cols =
        let allProb = findAllProbIndex tbIdxes []
         in findBestIndex allProb 0 Nothing
        where
            colNames = map (\(TableColumn _ cn) -> cn) cols

            findAllProbIndex [] res = res
            findAllProbIndex (i:is) res =
                if   all (`elem` colNames) $ snd i
                then findAllProbIndex is $ i : res
                else findAllProbIndex is res

            findBestIndex [] _ res = res
            findBestIndex (i:is) len res =
                let len' = length $ snd i
                 in if   len' > len
                    then findBestIndex is len' $ Just i
                    else findBestIndex is len res

    splitEqPairToMkKey :: [(Expr, Expr)] -> [TableMetadata] -> ([(Int, String, [Expr])], [Expr])
    splitEqPairToMkKey pairs mds =
        let groupedPair  = fromMaybe [] $ groupEqPairByTable pairs mds
            idxToUse     = map (\(a, b) -> findBestIndexInColumnList (getTbIdx a) $ map fst b) groupedPair
            (key, cmbd)  = unzip $ zipWith reSplitPair idxToUse groupedPair
            indexKey     = map (\(Just x) -> x)
                         $ filter (\case {Nothing -> False; _ -> True}) key
         in (indexKey, concat cmbd)
        where
            getTbIdx i = metadata_index $ mds !! i

            connEqPair = uncurry $ BinExpr Eq

            -- split eq-pair to (maybe (tbIndex, idxName, key), combined-eq-pair)
            reSplitPair :: Maybe TableIndex -> (Int, [(Expr, Expr)]) -> (Maybe (Int, String, [Expr]), [Expr])
            reSplitPair Nothing x = (Nothing, map connEqPair $ snd x)
            reSplitPair (Just (idxName, cns)) ps =
                let ps' = filter inIndex $ snd ps
                    key = map (\cn -> snd . head $ dropWhile (\(TableColumn _ cn', _) -> cn' /= cn) ps') cns
                 in (Just (fst ps, idxName, key), map connEqPair $ filter (not . inIndex) $ snd ps)
                where
                    inIndex (TableColumn _ cn, _) = cn `elem` cns
                    inIndex _ = False

cSelectWrapper :: Select -> SelectResultType -> CodeGenEnv
cSelectWrapper a b = getMetadata >>= \mds
    -> resetMetadata
    >> cSelect a b
    >> putMetadata mds
    >> removeTemp


cIndexActionWrapper :: IndexAction -> CodeGenEnv
cIndexActionWrapper = cIndexAction


cTableActionWrapper :: TableActon -> CodeGenEnv
cTableActionWrapper = cTableAction


cDeleteWrapper :: Delete -> CodeGenEnv
cDeleteWrapper d = cDelete d >> removeTemp

cInsertWrapper :: Insert -> CodeGenEnv
cInsertWrapper = cInsert

cUpdateWrapper :: Update -> CodeGenEnv
cUpdateWrapper = cUpdate