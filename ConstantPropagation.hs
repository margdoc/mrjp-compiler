module ConstantPropagation where

import qualified Data.Map as Map

import IntermediateTypes (Program, ControlGraph (..), Block, VarName, Label, Statement (..), Value (..), replaceValue, BinaryOpType (..), UnaryOpType (..), mapNames, buildEdges)

toDelete :: VarName
toDelete = "%%%%%toDelete"

run :: Program -> Program
run = Map.map runForGraph
    where
        runForGraph :: ControlGraph -> ControlGraph
        runForGraph graph = propagatedGraph
            where
                propagatedGraph :: ControlGraph
                propagatedGraph = buildEdges $ graph
                    { graphData = runUntilNoChanges $ graphData graph
                    }

                -- emptyBlocks :: [Label]
                -- emptyBlocks = map fst $ filter (\(_, block) -> case block of
                --         [Goto _] -> True
                --         _ -> False
                --     ) $ Map.toList $ graphData propagatedGraph

                -- optimizedGraph :: ControlGraph
                -- optimizedGraph = foldr removeBlock propagatedGraph emptyBlocks

                -- removeBlock :: Label -> ControlGraph -> ControlGraph
                -- removeBlock label graph' = buildEdges $ graph'
                --     { graphData = Map.insert successor newSuccessorBlock $ Map.delete label $ foldr (Map.adjust (forwardGoto label successor)) (graphData graph') precursors
                --     , graphEntry = if graphEntry graph' == label then successor else graphEntry graph'
                --     }
                --     where
                --         precursors = Map.findWithDefault [] label (graphRevertedEdges graph')
                --         successor = case graphData graph' Map.! label of
                --             [Goto l] -> l
                --             _ -> error "removeBlock: block is not empty"

                --         recalcualeSuccessorPhi :: VarName -> [(Value, Label)] -> Statement
                --         recalcualeSuccessorPhi varName values = case values of
                --             [] -> error "recalcualeSuccessorPhi: empty list"
                --             [(v, l)] | l == label -> Assign varName v
                --             _ -> Phi varName $ filter (\(_, l) -> l /= label) values

                --         calcNewSuccessorBlock :: Block -> Block
                --         calcNewSuccessorBlock block = case block of
                --             (Phi varName values):block' -> recalcualeSuccessorPhi varName values : calcNewSuccessorBlock block'
                --             _ -> block
                --         newSuccessorBlock = calcNewSuccessorBlock $ graphData graph' Map.! successor


                -- forwardGoto :: Label -> Label -> Block -> Block
                -- forwardGoto from to block = reverse $ case reverse block of
                --     Goto l : rest
                --         | l == from -> Goto to : rest
                --     If cond l1 l2 : rest
                --         | l1 == from -> If cond to l2 : rest
                --         | l2 == from -> If cond l1 to : rest
                --     _ -> block


        runUntilNoChanges :: Map.Map Label Block -> Map.Map Label Block
        runUntilNoChanges blocks = runUntilNoChanges' env newBlocks
            where
                (env, newBlocks) = runForBlocks Map.empty blocks
                runUntilNoChanges' :: Map.Map VarName Int -> Map.Map Label Block -> Map.Map Label Block
                runUntilNoChanges' env' blocks'
                    | Map.null env' = blocks'
                    | otherwise = uncurry runUntilNoChanges' $ runForBlocks env' blocks'

        runForBlocks :: Map.Map VarName Int -> Map.Map Label Block -> (Map.Map VarName Int, Map.Map Label Block)
        runForBlocks globalEnv blocks =
            let (newEnv, newBlocks) = Map.mapAccum (\acc block ->
                        let (acc', block') = runForBlock globalEnv block in (Map.union acc acc', reverse block')
                    ) Map.empty blocks
            in (Map.difference newEnv globalEnv, newBlocks)

        runForBlock :: Map.Map VarName Int -> Block -> (Map.Map VarName Int, Block)
        runForBlock acc = foldl runForStatement (acc, [])

        runForStatement, runForStatement' :: (Map.Map VarName Int, Block) -> Statement -> (Map.Map VarName Int, Block)
        runForStatement (env, block) statement = runForStatement' (env, block) statement'
            where
                statement' = foldr (\(name, value) ->
                        mapNames (\v -> if v == name then toDelete else v) .
                        replaceValue (Variable name) (Constant value) .
                        replaceValue (Object name) (Constant value)
                    ) statement $ Map.toList env

        calcBinaryOp :: BinaryOpType -> Int -> Int -> Int
        calcBinaryOp Add = (+)
        calcBinaryOp Sub = (-)
        calcBinaryOp Mul = (*)
        calcBinaryOp Div = div
        calcBinaryOp Mod = mod
        calcBinaryOp Equal = \x y -> if x == y then 1 else 0
        calcBinaryOp NotEqual = \x y -> if x /= y then 1 else 0
        calcBinaryOp Less = \x y -> if x < y then 1 else 0
        calcBinaryOp LessEqual = \x y -> if x <= y then 1 else 0
        calcBinaryOp Greater = \x y -> if x > y then 1 else 0
        calcBinaryOp GreaterEqual = \x y -> if x >= y then 1 else 0
        calcBinaryOp StringEqual = error "StringEqual is not supported in constant propagation"
        calcBinaryOp StringNotEqual = error "StringNotEqual is not supported in constant propagation"
        calcBinaryOp Concat = error "Concat is not supported in constant propagation"

        calcUnaryOp :: UnaryOpType -> Int -> Int
        calcUnaryOp Neg = negate
        calcUnaryOp Not = \x -> if x == 0 then 1 else 0

        runForStatement' (env, block) stmt@(AddRef name)
            | name == toDelete = (env, block)
            | otherwise = (env, stmt : block)
        runForStatement' (env, block) stmt@(RemoveRef name)
            | name == toDelete = (env, block)
            | otherwise = (env, stmt : block)
        runForStatement' (env, block) (Assign name (Constant value)) = (Map.insert name value env, block)
        runForStatement' (env, block) stmt@(BinaryOp op _ _ _) | op `elem` [Concat, StringEqual, StringNotEqual] = (env, stmt : block)
        runForStatement' (env, block) (BinaryOp op name (Constant c1) (Constant 0)) | op `elem` [Div, Mod] = (env, BinaryOp op name (Constant c1) (Variable "error_var") : Call (Just "error_var") "__dividing_by_zero" [] : block)
        runForStatement' (env, block) (BinaryOp op name (Constant value1) (Constant value2)) = (Map.insert name (calcBinaryOp op value1 value2) env, block)
        runForStatement' (env, block) (UnaryOp op name (Constant value)) = (Map.insert name (calcUnaryOp op value) env, block)
        runForStatement' (env, block) (If (Constant value) trueLabel falseLabel) = (env, Goto (if value == 1 then trueLabel else falseLabel) : block)
        runForStatement' (env, block) stmt = (env, stmt : block)
