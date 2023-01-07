{-# LANGUAGE LambdaCase #-}
module UnusedCode where

import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Block, Statement (..), buildEdges, Label)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = if f x then x : takeUntil f xs else [x]

run :: Program -> Program
run = Map.map removeUnusedBlocks
    where
        removeUnusedBlocks :: ControlGraph -> ControlGraph
        removeUnusedBlocks graph = buildEdges $ graph
            { graphData = Map.map removeUnusedStatements $ graphData graph
            }

        removeUnusedStatements :: Block -> Block
        removeUnusedStatements = takeUntil (\case
            Return{} -> False
            VReturn -> False
            _ -> True)

        removeUnreachableBlocks :: ControlGraph -> ControlGraph
        removeUnreachableBlocks graph = graph
            { graphData = Map.filterWithKey (\k _ -> Set.member k reachable) $ graphData graph
            }
            where
                reachable :: Set.Set Label
                reachable = reachableFrom (graphEntry graph) Set.empty

                successors :: Label -> [Label]
                successors label = Map.findWithDefault [] label $ graphEdges graph

                reachableFrom :: Label -> Set.Set Label -> Set.Set Label
                reachableFrom label visited = if Set.member label visited
                    then visited
                    else foldr reachableFrom (Set.insert label visited) $ successors label
