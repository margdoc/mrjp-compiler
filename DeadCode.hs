module DeadCode where

import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Block, VarName, varNames, Label, isPure)


run :: Program -> Program
run = Map.map removeDeadCodeFromGraph
    where
        removeDeadCodeFromGraph :: ControlGraph -> ControlGraph
        removeDeadCodeFromGraph graph = graph
            { graphData = Map.mapWithKey (\k -> removeDeadCode (snd $ inOuts Map.! k)) $ graphData graph
            }
            where
                inOuts = calcInOut graph

        removeDeadCode :: Set.Set VarName -> Block -> Block
        removeDeadCode outs' block = snd $ foldr (\statement (o, b) ->
                let (def, vars) = varNames statement in
                    case (def, isPure statement) of
                        (Nothing, _) -> (o `Set.union` Set.fromList vars, statement : b)
                        (Just def', p) -> if def' `Set.member` o || not p then
                                (Set.fromList vars `Set.union` Set.delete def' o, statement : b)
                            else
                                (o, b)
            ) (outs', []) block

        usesKills :: Block -> (Set.Set VarName, Set.Set VarName)
        usesKills = foldr (\statement (uses, kills) ->
                let (def, vars) = varNames statement in
                    (Set.union uses (Set.difference (Set.fromList vars) kills), kills `Set.union` maybe Set.empty Set.singleton def)
            ) (Set.empty, Set.empty) . reverse

        calcInOut :: ControlGraph -> Map.Map Label (Set.Set VarName, Set.Set VarName)
        calcInOut graph = fixPoint (calcInOutStep graph) $ Map.map (const (Set.empty, Set.empty)) $ graphData graph
            where
                fixPoint :: Eq a => (a -> a) -> a -> a
                fixPoint f x = if x == x' then x else fixPoint f x'
                    where
                        x' = f x

                calcInOutStep :: ControlGraph -> Map.Map Label (Set.Set VarName, Set.Set VarName) -> Map.Map Label (Set.Set VarName, Set.Set VarName)
                calcInOutStep g previous = Map.mapWithKey calcForBlock $ graphData g
                    where
                        calcForBlock :: Label -> Block -> (Set.Set VarName, Set.Set VarName)
                        calcForBlock label block = (inSs, outSs)
                            where
                                (_, prevOut) = previous Map.! label
                                (useB, killB) = usesKills block
                                inSs = Set.union useB (Set.difference prevOut killB)
                                outSs = Set.unions $ map (\l -> fst $ previous Map.! l) $ Map.findWithDefault [] label $ graphEdges graph
