{-# LANGUAGE LambdaCase #-}
module UnusedCode where

import qualified Data.Map as Map

import IntermediateTypes (Program, ControlGraph (..), Block, Statement (..), buildEdges)

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
