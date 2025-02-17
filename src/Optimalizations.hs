module Optimalizations where

import qualified Data.Map as Map

import IntermediateTypes (Program)

import qualified UnusedCode
import qualified ConstantPropagation
import qualified LCSE_GCSE
import qualified DeadCode
import qualified UnusedBlocks


type OptimalizationLevel = Int

defaultLevel :: OptimalizationLevel
defaultLevel = 2

data Optimalization = UnusedCode
                    | ConstantPropagation
                    | LCSE_GCSE
                    | DeadCode
                    | UnusedBlocks

optimalizationLevels :: Map.Map OptimalizationLevel [Optimalization]
optimalizationLevels = Map.fromList
    [ (0, [])
    , (1, [UnusedCode])
    , (2, [UnusedCode, LCSE_GCSE, DeadCode])
    , (3, [UnusedCode, LCSE_GCSE, DeadCode, ConstantPropagation, UnusedBlocks])
    ]

runOptimalization :: Optimalization -> Program -> Program
runOptimalization UnusedCode program = UnusedCode.run program
runOptimalization ConstantPropagation program = ConstantPropagation.run program
runOptimalization LCSE_GCSE program = LCSE_GCSE.run program
runOptimalization DeadCode program = DeadCode.run program
runOptimalization UnusedBlocks program = UnusedBlocks.run program


run :: OptimalizationLevel -> Program -> Program
run level = optimize
    where
        optimize prog = if prog == optimized then optimized else optimize optimized
            where
                optimized = foldl (flip runOptimalization) prog $ optimalizationLevels Map.! level
