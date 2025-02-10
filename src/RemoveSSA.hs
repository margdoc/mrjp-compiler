module RemoveSSA where

import qualified Data.Map as Map

import IntermediateTypes (Program, ControlGraph(..), Statement(..), mapNames, VarName, Value (..), Block, buildEdges, Label, varNameFromValue)
import Data.List (partition)

removePhiSuffix :: String -> String
removePhiSuffix = takeWhile (/= '@')

transform :: Program -> Program
transform = Map.map (transformGraph . buildEdges)
    where
        transformGraph :: ControlGraph -> ControlGraph
        transformGraph graph = graph
            { graphData = let (phis, blocks) = transformBlocks $ graphData graph
                          in addToBlocks phis blocks
            }

        addToBlocks :: Map.Map Label [(VarName, Value)] -> Map.Map Label Block -> Map.Map Label Block
        addToBlocks phis = Map.mapWithKey (\label block -> foldr (\(varName, value) b -> addToBlock varName value b) block (Map.findWithDefault [] label phis))
            where
                addToBlock :: VarName -> Value -> Block -> Block
                addToBlock varName value block = reverse $ case reverse block of
                    (stmt@(Goto{}) : rest) -> stmt : Assign varName value : rest
                    (stmt@(If{}) : rest) -> stmt : Assign varName value : rest
                    _ -> error "addToBlock: error"

        transformBlocks :: Map.Map Label Block -> (Map.Map Label [(VarName, Value)], Map.Map Label Block)
        transformBlocks = Map.mapAccumWithKey (\acc _ block ->
                let (block', phis) = transformBlock block
                    phis' = map dataFromPhi phis
                in (foldr (\(varName, values) acc' ->
                        foldr (\(value, label) acc'' -> insertIfNotSame label varName value acc'') acc' values) acc phis', block')
            ) Map.empty

        transformBlock :: Block -> (Block, [Statement])
        transformBlock = partition isNotPhi . map transformStatement

        transformStatement :: Statement -> Statement
        transformStatement = mapNames removePhiSuffix

insertIfNotSame :: Label -> VarName -> Value -> Map.Map Label [(VarName, Value)] -> Map.Map Label [(VarName, Value)]
insertIfNotSame label varName value = case varNameFromValue value of
    Just varName' -> if removePhiSuffix varName == removePhiSuffix varName'
        then id
        else Map.insertWith insertIfNotExist label [(varName, value)]
    Nothing -> Map.insertWith insertIfNotExist label [(varName, value)]
    where
        insertIfNotExist :: [(VarName, Value)] -> [(VarName, Value)] -> [(VarName, Value)]
        insertIfNotExist new old = if any (\(varName', value') -> varName == varName' && value == value') old
            then old
            else new ++ old


dataFromPhi :: Statement -> (VarName, [(Value, Label)])
dataFromPhi (Phi varName values) = (varName, values)
dataFromPhi _ = error "Not a phi"

check :: Program -> Bool
check = all checkGraph . Map.elems
    where
        checkGraph :: ControlGraph -> Bool
        checkGraph = all (all isNotPhi) . Map.elems . graphData

isNotPhi :: Statement -> Bool
isNotPhi (Phi{}) = False
isNotPhi _ = True
