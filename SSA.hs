{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module SSA where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

import IntermediateTypes (Program, ControlGraph (..), Label, Block, VarName, Statement (..), Value (..), varNames, renameOutput, mapNames, varNameFromValue)


transform :: Program -> Program
transform = Map.map transformFunction

transformFunction :: ControlGraph -> ControlGraph
transformFunction controlGraph = controlGraph
    { graphData = transformBlocks controlGraph $ graphData controlGraph
    }

transformBlocks :: ControlGraph -> Map.Map Label Block -> Map.Map Label Block
transformBlocks controlGraph blocks = Map.mapWithKey (\label (block, _, _) -> replaceVariable label block) variablesInBlocks
    where
        variableVersionName :: Label -> VarName -> Int -> VarName
        variableVersionName label varName version = varName ++ "@" ++ label ++ "^" ++ show version

        gatherAllVariables :: Label -> Block -> (Block, Map.Map VarName Int, [VarName])
        gatherAllVariables label block = (reverse block', defined, Set.toList missing)
            where
                argsIfEntry = Map.fromList $ if label == graphEntry controlGraph
                    then map (,0) $ graphArgs controlGraph
                    else []

                (block', defined, missing) = foldl gatherAllVariables' ([], argsIfEntry, Set.empty) block

                gatherAllVariables' :: ([Statement], Map.Map VarName Int, Set.Set VarName) -> Statement -> ([Statement], Map.Map VarName Int, Set.Set VarName)
                gatherAllVariables' (newBlock, defined', missing') stmt = (newStmt' : newBlock, newDefined, newMissing)
                    where
                        varNames' = varNames stmt
                        newMissing = foldr (\varName acc -> if Map.member varName defined'
                                    then acc
                                    else Set.insert varName acc
                                ) missing' $ snd varNames'
                        (newStmt, newDefined) = case fst varNames' of
                            Just varName -> case Map.lookup varName defined' of
                                Just version -> (replaceVariableInStatement varName (version+1) stmt, Map.insert varName (version+1) defined')
                                Nothing -> (replaceVariableInStatement varName 1 stmt, Map.insert varName 1 defined')
                            Nothing -> (stmt, defined')

                        replaceVariableInStatement :: VarName -> Int -> Statement -> Statement
                        replaceVariableInStatement varName version stmt' =
                            renameOutput varName newName $
                            mapNames (\name -> if name == varName then newName else name) stmt'
                            where
                                newName = variableVersionName label varName version

                        newStmt' = foldr (\varName stmt' -> case Map.lookup varName defined' of
                                Just version -> replaceVariableInStatement varName version stmt'
                                Nothing -> stmt'
                            ) newStmt $ snd varNames'

        variablesInBlocks :: Map.Map Label (Block, Map.Map VarName Int, [VarName])
        variablesInBlocks = Map.mapWithKey gatherAllVariables blocks

        importedVariables :: Map.Map Label (Map.Map VarName (Map.Map Label (Label, Int)))
        importedVariables = Map.mapWithKey (\label (_, _, missing) -> Map.fromList $ map (\v -> (v, dfsImportVariables label v Set.empty Map.empty)) missing) variablesInBlocks

        dfsImportVariables, dfsImportVariables' :: Label -> VarName -> Set.Set Label -> Map.Map Label (Label, Int) -> Map.Map Label (Label, Int)
        dfsImportVariables label varName visited acc' = if Set.member label visited
            then acc'
            else foldr (\label' acc ->
                case Map.lookup varName ((\(_, a, _) -> a) $ variablesInBlocks Map.! label') of
                    Just i -> Map.insert label' (label', i) acc
                    Nothing -> if varName `elem` (\(_, _, a) -> a) (variablesInBlocks Map.! label')
                        then Map.insert label (label', 0) acc
                        else dfsImportVariables' label' varName (Set.insert label visited) acc
                ) acc' $ Map.findWithDefault [] label (graphRevertedEdges controlGraph)

        dfsImportVariables' label varName visited acc' = if Set.member label visited
            then acc'
            else foldr (\label' acc ->
                case Map.lookup varName ((\(_, a, _) -> a) $ variablesInBlocks Map.! label') of
                    Just i -> Map.insert label' (label', i) acc
                    Nothing -> dfsImportVariables' label' varName (Set.insert label visited) acc
                ) acc' $ Map.findWithDefault [] label (graphRevertedEdges controlGraph)

        replaceVariable :: Label -> Block -> Block
        replaceVariable label block = nonSingletonPhis ++ block''
            where
                phiStatements = map (\varName ->
                        Phi (variableVersionName label varName 0) $
                            map (\(labelFrom, (label', version)) -> (Variable $ variableVersionName label' varName version, labelFrom))
                                $ Map.toList $ Map.findWithDefault Map.empty varName $ importedVariables Map.! label
                    ) $ (\(_, _, a) -> a) $ variablesInBlocks Map.! label
                block' = map (\stmt -> foldr (\varName stmt' -> let newName = variableVersionName label varName 0 in
                            if elem varName $ (\(_, _, a) -> a) $ variablesInBlocks Map.! label
                            then mapNames (\name -> if name == varName then newName else name) stmt'
                            else stmt'
                        ) stmt $ snd $ varNames stmt
                    ) block
                singletonPhis = filter (\case
                        Phi _ [(_, _)] -> True
                        _ -> False
                    ) phiStatements
                singletonPhisValues = map (\case
                        Phi new [(previous, _)] -> (new, previous)
                        _ -> error "This should not happen"
                    ) singletonPhis
                nonSingletonPhis = filter (\case
                        Phi _ [_, _] -> True
                        _ -> False
                    ) phiStatements
                block'' = map (\stmt -> foldr (\(new, previous) stmt' -> let oldName = fromJust $ varNameFromValue previous in
                        mapNames (\name -> if name == new then oldName else name) stmt') stmt singletonPhisValues) block'
