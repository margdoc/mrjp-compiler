module Optimalizations where

import qualified Data.Map as Map

import IntermediateTypes (Program)

import qualified UnusedCode
import qualified ConstantPropagation
import qualified LCSE_GCSE

type OptimalizationLevel = Int

defaultLevel :: OptimalizationLevel
defaultLevel = 1

data Optimalization = UnusedCode
                    | ConstantPropagation
                    | LCSE_GCSE

optimalizationLevels :: Map.Map OptimalizationLevel [Optimalization]
optimalizationLevels = Map.fromList
    [ (0, [])
    , (1, [UnusedCode, ConstantPropagation])
    , (2, [UnusedCode, ConstantPropagation, LCSE_GCSE])
    ]

runOptimalization :: Optimalization -> Program -> Program
runOptimalization UnusedCode program = UnusedCode.run program
runOptimalization ConstantPropagation program = ConstantPropagation.run program
runOptimalization LCSE_GCSE program = LCSE_GCSE.run program


run :: OptimalizationLevel -> Program -> Program
run level = optimize
    where
        optimize prog = if prog == optimized then optimized else optimize optimized
            where
                optimized = foldl (flip runOptimalization) prog $ optimalizationLevels Map.! level
