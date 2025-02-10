{-# LANGUAGE LambdaCase #-}

module LCSE_GCSE where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Block, BinaryOpType (..), Value (..), UnaryOpType (..), VarName, Statement (..), varNames, mapNames, Label, buildEdges, varNameFromValue)

data Operation = BinaryOperation BinaryOpType Value Value
               | UnaryOperation UnaryOpType Value
               | SelfOperation
               | LengthOperation Value
                deriving (Eq, Show, Ord)


type AliasingData = (Map.Map VarName VarName, Map.Map Operation VarName)


removePhiSuffix :: VarName -> VarName
removePhiSuffix = takeWhile (/= '@')

operationVars :: Operation -> [VarName]
operationVars = Maybe.mapMaybe varNameFromValue . \case
    BinaryOperation _ v1 v2 -> [v1, v2]
    UnaryOperation _ v -> [v]
    SelfOperation -> []
    LengthOperation v -> [v]

depends :: VarName -> Operation -> Bool
depends var op = removePhiSuffix var `elem` map removePhiSuffix (operationVars op)

clearOperations :: VarName -> Map.Map Operation VarName -> Map.Map Operation VarName
clearOperations var = Map.filterWithKey (\k _ -> not $ depends var k)


addAlias :: VarName -> VarName -> AliasingData -> AliasingData
addAlias newVar oldVar (aliases, operations) = (Map.insert oldVar newVar aliases', operations)
    where
        aliases' = Map.map (\v -> if v == oldVar then newVar else v) aliases


run :: Program -> Program
run = Map.map (runForGraph . buildEdges)
    where
        runForGraph :: ControlGraph -> ControlGraph
        runForGraph graph = newGraphData
            where
                newGraphData = dfs Set.empty (Map.empty, Map.empty) (graphEntry graph) graph

        dfs :: Set.Set Label -> AliasingData -> Label -> ControlGraph -> ControlGraph
        dfs visited aliasingData label graph = if Set.member label visited then graph else
            foldr (dfs newVisited newAliasingData) newGraph (Map.findWithDefault [] label (graphEdges graph))
            where
                newVisited = Set.insert label visited
                block = graphData graph Map.! label
                (newAliasingData, newBlock) = runForBlock aliasingData block
                newGraph = graph { graphData = Map.insert label newBlock $ graphData graph }

        runForBlock :: AliasingData -> Block -> (AliasingData, Block)
        runForBlock aliasingData block = let (a, b) = foldl runForStatement (aliasingData, []) block in (a, reverse b)

        aliasStmt :: Map.Map VarName VarName -> Statement -> Statement
        aliasStmt aliases = mapNames (\v -> Maybe.fromMaybe v (Map.lookup v aliases))

        runForStatement, runForStatement' :: (AliasingData, Block) -> Statement -> (AliasingData, Block)
        runForStatement ((aliases, operations), block) stmt = runForStatement' (newAliasingData, block) $ aliasStmt aliases stmt
            where
                newAliasingData = case fst $ varNames stmt of
                    Just v -> (Map.filter ((/=) (removePhiSuffix v) . removePhiSuffix) aliases, clearOperations v operations)
                    Nothing -> (aliases, operations)

        addOperation :: Operation -> VarName -> Statement -> (AliasingData, Block) -> (AliasingData, Block)
        addOperation operation tmp stmt ((aliases, operations), block) = case Map.lookup operation operations of
            Nothing -> ((aliases, Map.insert operation tmp operations), stmt : block)
            Just oldTmp -> (addAlias oldTmp tmp (aliases, operations), stmt : block)

        runForStatement' acc@((aliases, operations), block) = \case
            stmt@(Assign oldVar (Variable newVar)) -> (addAlias newVar oldVar (aliases, operations), stmt : block)
            stmt@(Assign oldVar (Object newVar)) -> (addAlias newVar oldVar (aliases, operations), stmt : block)
            stmt@(BinaryOp op tmp v1 v2) -> addOperation (BinaryOperation op v1 v2) tmp stmt acc
            stmt@(UnaryOp op tmp v) -> addOperation (UnaryOperation op v) tmp stmt acc
            stmt@(Self tmp) -> addOperation SelfOperation tmp stmt acc
            stmt@(ArrayLength tmp v) -> addOperation (LengthOperation v) tmp stmt acc
            stmt -> ((aliases, operations), stmt : block)
