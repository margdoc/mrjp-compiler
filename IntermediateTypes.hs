module IntermediateTypes where

import Control.Monad.RWS ( RWS )
import qualified Data.Map as Map
import BasePrelude (intercalate)

type Compiler a = RWS Env Output State (IO a)

type Env = ()

type Output = ()

type State = ()

type VarName = String

data Value = Constant Int
           | Variable VarName
           | Object VarName

instance Show Value where
    show (Constant int) = show int
    show (Variable varName) = varName
    show (Object varName) = "&" ++ varName

type Label = String

data FunctionLabel = FunctionLabel Label
                   | Pointer Value

instance Show FunctionLabel where
    show (FunctionLabel label) = label
    show (Pointer value) = "*" ++ show value

data BinaryOpType = Add
                  | Sub
                  | Mul
                  | Div
                  | Mod
                  | Equal
                  | NotEqual
                  | Less
                  | LessEqual
                  | Greater
                  | GreaterEqual
                  | Concat
                  deriving (Eq)

instance Show BinaryOpType where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Equal = "=="
    show NotEqual = "!="
    show Less = "<"
    show LessEqual = "<="
    show Greater = ">"
    show GreaterEqual = ">="
    show Concat = "++"

data UnaryOpType = Not
                 | Neg

instance Show UnaryOpType where
    show Not = "!"
    show Neg = "-"

data Statement = BinaryOp BinaryOpType VarName Value Value
               | UnaryOp UnaryOpType VarName Value
               | Phi VarName [(Value, Label)]
               | If Value Label Label
               | Load VarName Value Value     -- value1 = value2[value3]
               | Store VarName Value Value    -- value1[value2] = value3
               | Assign VarName Value
               | AssignString VarName String
               | Call VarName FunctionLabel [Value]   -- function_label arguments...
               | Goto Label
               | Return Value
               | VReturn
               | AddRef VarName
               | RemoveRef VarName

instance Show Statement where
    show (BinaryOp op varName value1 value2) = varName ++ " = " ++ show value1 ++ " " ++ show op ++ " " ++ show value2
    show (UnaryOp op varName value) = varName ++ " = " ++ show op ++ " " ++ show value
    show (Phi varName values) = varName ++ " = phi " ++ show values
    show (If value label1 label2) = "if " ++ show value ++ " goto " ++ label1 ++ " else goto " ++ label2
    show (Load varName value1 value2) = varName ++ " = " ++ show value1 ++ "[" ++ show value2 ++ "]"
    show (Store varName value1 value2) = show varName ++ "[" ++ show value1 ++ "] = " ++ show value2
    show (Assign varName value) = varName ++ " = " ++ show value
    show (AssignString varName string) = varName ++ " = " ++ show string
    show (Call varName functionLabel values) = varName ++ " = " ++ show functionLabel ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (Goto label) = "goto " ++ label
    show (Return value) = "return " ++ show value
    show VReturn = "return"
    show (AddRef varName) = "++" ++ varName
    show (RemoveRef varName) = "--" ++ varName

type Block = [Statement]

data ControlGraph = ControlGraph
    { graphData :: Map.Map Label Block
    , graphEdges :: Map.Map Label [Label]
    , graphEntry :: Label
    , graphArgs :: [VarName]
    }

instance Show ControlGraph where
    show (ControlGraph graphData' graphEdges' graphEntry' args) =
        "ControlGraph:\nentry: " ++ graphEntry' ++
        "\nedges: " ++ show graphEdges' ++
        "\nargs: " ++ show args ++
        "\ndata:\n" ++ unlines (map (\(label, block) -> "Label " ++ label ++ ":\n" ++ unlines (map (("  " ++) . show) block)) $ Map.assocs graphData')

type Program = Map.Map Label ControlGraph
