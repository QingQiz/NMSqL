{-# LANGUAGE LambdaCase #-}
module Generator.Insert (cInsert) where


import Ast
import Instruction
import TableMetadata
import CodeGeneratorUtils
import {-# SOURCE #-} CodeGenerator

import Data.List
import Control.Monad.Except


cInsert :: Insert -> CodeGenEnv
cInsert (Insert tbName cols (ValueList values)) = getMetadata >>= \mds
    -> if not (tbExists tbName mds) then throwError ("no such table: " ++ tbName) else doNothing
    >> appendInstructions [Instruction opTransaction  0            0 ""
                          ,Instruction opVerifyCookie (cookie mds) 0 ""]
    >> let md = head $ dropWhile ((/=tbName) . metadata_name) mds
           valLen  = length values
           colLen  = length cols
           colLen' = length (metadata_column md)
        in if   (colLen == 0 && colLen' /= valLen) || (colLen /= 0 && colLen /= valLen)
           then throwError $ show valLen ++ " value(s) for "
                          ++ show (if colLen == 0 then colLen' else colLen) ++ " column(s)"
           else let values' = case cols of
                        [] -> zip (metadata_column md) $ map (\(ConstValue v) -> v) values
                        _  -> let buildValList (x:xs) res = case elemIndex x cols of
                                      Nothing -> buildValList xs ((x, Null) : res)
                                      Just  i -> buildValList xs
                                                     ((x, (\(ConstValue v) -> v) (values !! i)) : res)
                                  buildValList [] res = res
                               in buildValList (metadata_column md) []

                    getColVal colNames =
                        let buildVal = \case
                                Null        -> Instruction opNull    0 0 ""
                                ValInt    a -> Instruction opInteger a 0 ""
                                ValDouble a -> Instruction opString  0 0 $ show a
                                ValStr    a -> Instruction opString  0 0 a

                            getVal colName = case dropWhile ((/=colName) . fst) values' of
                                [] -> throwError ("no such column: " ++ colName)
                                x  -> appendInstructions [buildVal $ snd $ head x]
                         in connectCodeGenEnv (map getVal colNames)

                    insertIntoIndex ((idxName, idxCol):xs) = getColVal idxCol
                        >> appendInstructions [Instruction opMakeKey   (length idxCol) 0 ""
                                              ,Instruction opOpenWrite 1               0 idxName
                                              ,Instruction opAddress   0               0 ""
                                              ,Instruction opPut       1               0 ""
                                              ,Instruction opClose     1               0 ""]
                        >> insertIntoIndex xs
                    insertIntoIndex [] = doNothing

                    checkCols (c:cs) = if snd (columnIdx c [md]) == -1
                                       then throwError $ "no such column: " ++ c
                                       else checkCols cs
                    checkCols [] = doNothing
                 in checkCols cols
                 >> appendInstructions [Instruction opOpenWrite  0 0 tbName
                                       ,Instruction opDefaultKey 0 0 ""]
                 >> getColVal (metadata_column md)
                 >> appendInstructions [Instruction opMakeRecord (length $ metadata_column md) 0 ""
                                       ,Instruction opPut 0 0 ""]
                 >> insertIntoIndex (metadata_index md)
    >> appendInstructions [Instruction opClose  0 0 ""
                          ,Instruction opCommit 0 0 ""]


cInsert (Insert tbName cols (SelectResult select)) = getMetadata >>= \mds
    -> if not (tbExists tbName mds) then throwError ("no such table: " ++ tbName) else doNothing
    >> appendInstructions [Instruction opTransaction  0            0 ""
                          ,Instruction opVerifyCookie (cookie mds) 0 ""]
    >> let md = head $ dropWhile ((/=tbName) . metadata_name) mds
           selResLen = case selectResult select of
               [(AnyColumn, _)] -> length
                                 $ concatMap metadata_column
                                 $ filter ((`elem` selectTableName select) . metadata_name) mds
               other            -> length other
           colLen  = length cols
           colLen' = length (metadata_column md)
        in if   (colLen == 0 && colLen' /= selResLen) || (colLen /= 0 && colLen /= selResLen)
           then throwError $ show selResLen ++ " value(s) for "
                          ++ show (if colLen == 0 then colLen' else colLen) ++ " column(s)"
           else let getColVal colNames values cr=
                        let getInst colName = case dropWhile ((/=colName) . fst) values of
                                []  -> appendInst opNull   0   0       ""
                                x:_ -> appendInst opColumn cr  (snd x) ""
                         in connectCodeGenEnv (map getInst colNames)

                    insertIntoIndex ((idxName,idxCol):xs) cr =
                        let values = zip (metadata_column md) [0..]
                         in getColVal idxCol values 0
                         >> appendInstructions [Instruction opMakeKey   (length idxCol) 0 ""
                                               ,Instruction opAddress   0               0 ""
                                               ,Instruction opOpenWrite cr              0 idxName
                                               ,Instruction opPut       cr              0 ""
                                               ,Instruction opClose     cr              0 ""]
                         >> insertIntoIndex xs cr
                    insertIntoIndex [] _ = doNothing

                    insertIntoTable tmpCr =
                        let values = zip cols [0..]
                         -- tmpCr can not be 0, so we use 0 to open table
                         in appendInstructions [Instruction opOpenWrite  0 0 tbName
                                               ,Instruction opDefaultKey 0 0 ""]
                         >> getColVal (metadata_column md) values tmpCr
                         >> appendInstructions [Instruction opMakeRecord colLen' 0 ""
                                               ,Instruction opPut        0       0 ""]

                 in appendEnv (cSelectWrapper select ToTemp)
                 >> (subtract 1 <$> getCursor) >>= \cr -> getLabel >>= \lab
                 -> appendInstructions
                        -- rewind temp table
                        [Instruction opRewind  cr  0         ""
                        ,Instruction opNoop    0   (lab + 1) ""]
                 -- insert into table
                 >> insertIntoTable cr
                 -- insert into index, use `cr + 1' to open index
                 >> insertIntoIndex (metadata_index md) (cr + 1)
                 >> appendInstructions
                        [Instruction opClose   0   0         ""
                        --
                        ,Instruction opNext    cr  lab       ""
                        ,Instruction opGoto    0   (lab + 1) ""
                        ,Instruction opNoop    0   lab       ""
                        ,Instruction opClose   cr  0         ""]
                 >> putLabel (lab + 2)
    >> appendInst opCommit 0 0 ""
