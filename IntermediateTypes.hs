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
               | Store Value Value Value    -- value1[value2] = value3
               | ArrayLength VarName Value
               | AllocArray VarName Value
               | Get VarName Value VarName VarName
               | Set Value VarName VarName Value
               | AllocObject VarName String
               | Self VarName
               | Assign VarName Value
               | AssignString VarName String
               | Call VarName Label [Value]   -- function_label arguments...
               | CallMethod VarName Value VarName Label [Value]
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
    show (Store value1 value2 value3) = show value1 ++ "[" ++ show value2 ++ "] = " ++ show value3
    show (ArrayLength varName value) = varName ++ " = " ++ show value ++ ".length"
    show (AllocArray varName value) = varName ++ " = new[" ++ show value ++ "]"
    show (Get varName value className varName') = varName ++ " = " ++ show value ++ "." ++ className ++ "." ++ varName'
    show (Set value className varName' value') = show value ++ "." ++ className ++ "." ++ varName' ++ " = " ++ show value'
    show (AllocObject varName string) = varName ++ " = new " ++ string
    show (Assign varName value) = varName ++ " = " ++ show value
    show (AssignString varName string) = varName ++ " = " ++ show string
    show (Call varName functionLabel values) = varName ++ " = " ++ functionLabel ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (CallMethod varName value className varName' values) = varName ++ " = " ++ show value ++ "." ++ className ++ "." ++ varName' ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (Goto label) = "goto " ++ label
    show (Return value) = "return " ++ show value
    show VReturn = "return"
    show (AddRef varName) = "++" ++ varName
    show (RemoveRef varName) = "--" ++ varName
    show (Self varName) = varName ++ " = self"

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

methodLabel :: VarName -> VarName -> Label
methodLabel className methodName = className ++ "." ++ methodName

data FunctionLabel = FunctionLabel Label
                   | MethodLabel VarName Label
