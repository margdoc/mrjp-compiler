{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module SSA where

import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Label, Block, VarName, Statement (..), Value (..), varNames, renameOutput, mapNames, buildEdges)


transform :: Program -> Program
transform = Map.map transformFunction

transformFunction :: ControlGraph -> ControlGraph
transformFunction controlGraph = controlGraph
    { graphData = transformBlocks controlGraph $ graphData controlGraph
    }

type Phis = Map.Map Label (Map.Map VarName (Map.Map Label (Label, Int)))

transformBlocks :: ControlGraph -> Map.Map Label Block -> Map.Map Label Block
transformBlocks controlGraph' blocks = Map.mapWithKey (\label _ -> replaceVariable label) variablesInBlocks
    where
        controlGraph = buildEdges controlGraph'

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
                                Just version -> (renameOutput varName (variableVersionName label varName (version+1)) stmt, Map.insert varName (version+1) defined')
                                Nothing -> (renameOutput varName (variableVersionName label varName 1) stmt, Map.insert varName 1 defined')
                            Nothing -> (stmt, defined')

                        replaceVariableInStatement :: VarName -> Int -> Statement -> Statement
                        replaceVariableInStatement varName version =
                            mapNames (\name -> if name == varName then newName else name)
                            where
                                newName = variableVersionName label varName version

                        newStmt' = foldr (\varName stmt' -> case Map.lookup varName defined' of
                                Just version -> replaceVariableInStatement varName version stmt'
                                Nothing -> if Set.member varName newMissing then replaceVariableInStatement varName 0 stmt' else stmt'
                            ) newStmt $ snd varNames'

        variablesInBlocks :: Map.Map Label (Block, Map.Map VarName Int, [VarName])
        variablesInBlocks = Map.mapWithKey gatherAllVariables blocks

        -- importedVariables :: Map.Map Label (Map.Map VarName (Map.Map Label (Label, Int)))
        -- importedVariables = Map.mapWithKey (\label (_, _, missing) -> Map.fromList $ map (\v -> (v, dfsImportVariables label v Set.empty Map.empty)) missing) variablesInBlocks

        -- dfsImportVariables :: Label -> VarName -> Set.Set Label -> Map.Map Label (Label, Int) -> Map.Map Label (Label, Int)
        -- dfsImportVariables label varName visited acc' = if Set.member label visited
        --     then acc'
        --     else foldr (\label' acc ->
        --         case Map.lookup varName ((\(_, a, _) -> a) $ variablesInBlocks Map.! label') of
        --             Just i -> Map.insert label' (label', i) acc
        --             Nothing -> if varName `elem` (\(_, _, a) -> a) (variablesInBlocks Map.! label')
        --                 then if Map.size precursorAcc == 1 then Map.union precursorAcc acc else Map.insert label (label', 0) acc
        --                 else dfsImportVariables' label' label' varName (Set.insert label visited) acc
        --                     where
        --                         precursorAcc = dfsImportVariables' label' label' varName (Set.insert label visited) Map.empty

        --         ) acc' $ Map.findWithDefault [] label (graphRevertedEdges controlGraph)

        -- dfsImportVariables' :: Label -> Label -> VarName -> Set.Set Label -> Map.Map Label (Label, Int) -> Map.Map Label (Label, Int)
        -- dfsImportVariables' basicLabel label varName visited acc' = if Set.member label visited
        --     then acc'
        --     else foldr (\label' acc ->
        --         case Map.lookup varName ((\(_, a, _) -> a) $ variablesInBlocks Map.! label') of
        --             Just i -> Map.insert label' (label', i) acc
        --             Nothing -> dfsImportVariables' basicLabel label' varName (Set.insert label visited) acc
        --         ) acc' $ Map.findWithDefault [] label (graphRevertedEdges controlGraph)

        initialPhis :: Phis
        initialPhis = foldr (\(label, (_, _, missing)) acc ->
                foldr (`addPhisForBlock` label) acc missing
            ) Map.empty $ Map.toList variablesInBlocks
            where
                addPhisForBlock :: VarName -> Label -> Phis -> Phis
                addPhisForBlock varName label acc = if Map.member varName $ Map.findWithDefault Map.empty label acc then acc
                    else foldr (\label' acc' -> case Map.lookup varName ((\(_, a, _) -> a) $ variablesInBlocks Map.! label') of
                            Just i -> insertPhi label' i acc'
                            Nothing -> addPhisForBlock varName label' (insertPhi label' 0 acc')
                        ) acc $ Map.findWithDefault [] label (graphRevertedEdges controlGraph)
                    where
                        insertPhi :: Label -> Int -> Phis -> Phis
                        insertPhi label' version = Map.insertWith (Map.unionWith Map.union) label (Map.singleton varName (Map.singleton label' (label', version)))


        -- type Phis = Map.Map Label (Map.Map VarName (Map.Map Label (Label, Int)))

        removePhi :: VarName -> Label -> (Label, Int) -> (Phis, Map.Map Label Block) -> (Phis, Map.Map Label Block)
        removePhi varName label (newLabel, newVersion) (phis, blocks') =
            (Map.map (Map.adjust (Map.map (\(l, i) -> if l == label && i == 0 then (newLabel, newVersion) else (l, i))) varName) $
             Map.adjust (Map.delete varName) label phis,
             Map.map (map (mapNames (\v ->
                    if v == oldVarName then variableVersionName newLabel varName newVersion else v
                ))) blocks')
            where
                oldVarName = variableVersionName label varName 0

        phisToRemove :: Phis -> [(VarName, Label, (Label, Int))]
        phisToRemove phis = concatMap (\(label, vars) ->
                map (\(varName, labels) -> (varName, label, head $ Map.elems labels)) $
                filter (\(_, labels) -> Set.size (Set.fromList $ Map.elems labels) == 1) $
                Map.toList vars
            ) $ Map.toList phis

        removePhisUntilFixedPoint :: Phis -> Map.Map Label Block -> (Phis, Map.Map Label Block)
        removePhisUntilFixedPoint phis oldBlocks = if null phisToRemove' then (phis, oldBlocks) else removePhisUntilFixedPoint newPhis newBlocks
            where
                phisToRemove' = phisToRemove phis
                (newPhis, newBlocks) = foldr (\(varName, label, (newLabel, newVersion)) ->
                        removePhi varName label (newLabel, newVersion)
                    ) (phis, oldBlocks) [head phisToRemove']

        finalPhis :: Phis
        finalBlocks :: Map.Map Label Block
        (finalPhis, finalBlocks) = removePhisUntilFixedPoint initialPhis (Map.map (\(b, _, _) -> b) variablesInBlocks)
        -- (finalPhis, finalBlocks) = (initialPhis, Map.map (\(b, _, _) -> b) variablesInBlocks)

        replaceVariable :: Label -> Block
        replaceVariable label = phiStatements ++ finalBlocks Map.! label
            where
                phiStatements = map (\(varName, varPhis) ->
                        Phi (variableVersionName label varName 0) $
                            map (\(labelFrom, (label', version)) -> (Variable $ variableVersionName label' varName version, labelFrom)) $ Map.toList varPhis
                    ) $ Map.toList $ Map.findWithDefault Map.empty label finalPhis
                -- block' = map (\stmt -> foldr (\varName stmt' -> let newName = variableVersionName label varName 0 in
                --             if elem varName $ (\(_, _, a) -> a) $ variablesInBlocks Map.! label
                --             then mapNames (\name -> if name == varName then newName else name) stmt'
                --             else stmt'
                --         ) stmt $ snd $ varNames stmt
                --     ) $ finalBlocks Map.! label
