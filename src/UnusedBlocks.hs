module UnusedBlocks where

import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Block, Statement (..), buildEdges, Label)

run :: Program -> Program
run = Map.map removeUnreachableBlocks
    where
        removeUnreachableBlocks :: ControlGraph -> ControlGraph
        removeUnreachableBlocks graph' = graph
            { graphData = case notReachable of
                Nothing -> graphData graph
                Just label -> graphData $ removeUnreachableBlocks $ fixPhis $ buildEdges $ removeBlock label graph
            }
            where
                graph :: ControlGraph
                graph = buildEdges graph'

                notReachable :: Maybe Label
                notReachable = case filter (\k -> not $ Set.member k reachable) $ Map.keys $ graphData graph of
                    [] -> Nothing
                    (k:_) -> Just k

                reachable :: Set.Set Label
                reachable = reachableFrom (graphEntry graph) Set.empty

                successors :: Label -> [Label]
                successors label = Map.findWithDefault [] label $ graphEdges graph

                reachableFrom :: Label -> Set.Set Label -> Set.Set Label
                reachableFrom label visited = if Set.member label visited
                    then visited
                    else foldr reachableFrom (Set.insert label visited) $ successors label

                removeBlock :: Label -> ControlGraph -> ControlGraph
                removeBlock label graph'' = graph''
                    { graphData = Map.delete label $ graphData graph''
                    }

                fixPhis :: ControlGraph -> ControlGraph
                fixPhis graph'' = graph''
                    { graphData = Map.map fixPhisForBlock $ graphData graph''
                    }
                    where
                        labels = Map.keysSet $ graphData graph''

                        fixPhisForBlock :: Block -> Block
                        fixPhisForBlock [] = []
                        fixPhisForBlock block@(statement:statements) = case statement of
                            Phi varName values -> case newValues of
                                [] -> fixPhisForBlock statements
                                [(value, _)] -> Assign varName value : fixPhisForBlock statements
                                _ -> Phi varName newValues : fixPhisForBlock statements
                                where
                                    newValues = filter (\(_, label) -> Set.member label labels) values
                            _ -> block
