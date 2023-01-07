{-# LANGUAGE LambdaCase #-}
module IntermediateTypes where

import BasePrelude (intercalate, mapMaybe)
import Control.Monad.RWS ( RWS )
import qualified Data.Map as Map

type Compiler a = RWS Env Output State (IO a)

type Env = ()

type Output = ()

type State = ()

type VarName = String

data Value = Constant Int
           | Variable VarName
           | Object VarName
           deriving (Eq, Ord)

instance Show Value where
    show (Constant int) = show int
    show (Variable varName) = varName
    show (Object varName) = '&' : varName

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
                  deriving (Eq, Ord)

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
                 deriving (Eq, Ord)

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
               | Call (Maybe VarName) Label [Value]   -- function_label arguments...
               | CallMethod (Maybe VarName) Value VarName Label [Value]
               | Goto Label
               | Return Value
               | VReturn
               | AddRef VarName
               | RemoveRef VarName
                deriving (Eq)

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
    show (Call Nothing functionLabel values) = functionLabel ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (Call (Just varName) functionLabel values) = varName ++ " = " ++ functionLabel ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (CallMethod Nothing value className varName' values) = show value ++ "." ++ className ++ "." ++ varName' ++ "(" ++ intercalate ", " (map show values) ++ ")"
    show (CallMethod (Just varName) value className varName' values) = varName ++ " = " ++ show value ++ "." ++ className ++ "." ++ varName' ++ "(" ++ intercalate ", " (map show values) ++ ")"
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
    , graphRevertedEdges :: Map.Map Label [Label]
    , graphEntry :: Label
    , graphArgs :: [VarName]
    }
    deriving (Eq)

instance Show ControlGraph where
    show (ControlGraph graphData' graphEdges' graphRevertedEdges' graphEntry' args) =
        "ControlGraph:\nentry: " ++ graphEntry' ++
        "\nedges: " ++ show graphEdges' ++
        "\nreverted edges: " ++ show graphRevertedEdges' ++
        "\nargs: " ++ show args ++
        "\ndata:\n" ++ unlines (map (\(label, block) -> "Label " ++ label ++ ":\n" ++ unlines (map (("  " ++) . show) block)) $ Map.assocs graphData')

type Program = Map.Map Label ControlGraph

methodLabel :: VarName -> VarName -> Label
methodLabel className methodName = className ++ "." ++ methodName

data FunctionLabel = FunctionLabel Label
                   | MethodLabel VarName Label

varNameFromValue :: Value -> Maybe VarName
varNameFromValue (Constant _) = Nothing
varNameFromValue (Variable varName) = Just varName
varNameFromValue (Object varName) = Just varName

varNames :: Statement -> (Maybe VarName, [VarName])
varNames (BinaryOp _ varName value1 value2) = (Just varName, mapMaybe varNameFromValue [value1, value2])
varNames (UnaryOp _ varName value) = (Just varName, mapMaybe varNameFromValue [value])
varNames (Phi varName values) = (Just varName, mapMaybe (varNameFromValue . fst) values)
varNames (If value _ _) = (Nothing, mapMaybe varNameFromValue [value])
varNames (Load varName value1 value2) = (Just varName, mapMaybe varNameFromValue [value1, value2])
varNames (Store value1 value2 value3) = (Nothing, mapMaybe varNameFromValue [value1, value2, value3])
varNames (ArrayLength varName value) = (Just varName, mapMaybe varNameFromValue [value])
varNames (AllocArray varName value) = (Just varName, mapMaybe varNameFromValue [value])
varNames (Get varName value _ _) = (Just varName, mapMaybe varNameFromValue [value])
varNames (Set value _ _ value') = (Nothing, mapMaybe varNameFromValue [value, value'])
varNames (AllocObject varName _) = (Just varName, [])
varNames (Assign varName value) = (Just varName, mapMaybe varNameFromValue [value])
varNames (AssignString varName _) = (Just varName, [])
varNames (Call varName _ values) = (varName, mapMaybe varNameFromValue values)
varNames (CallMethod varName value _ _ values) = (varName, mapMaybe varNameFromValue (value:values))
varNames (Goto _) = (Nothing, [])
varNames (Return value) = (Nothing, mapMaybe varNameFromValue [value])
varNames VReturn = (Nothing, [])
varNames (AddRef varName) = (Nothing, [varName])
varNames (RemoveRef varName) = (Nothing, [varName])
varNames (Self varName) = (Just varName, [])

replaceValueIf :: Value -> Value -> Value -> Value
replaceValueIf ifValue nextValue value = if ifValue == value then nextValue else value

replaceValue :: Value -> Value -> Statement -> Statement
replaceValue ifValue nextValue = \case
    BinaryOp op varName value1 value2 -> BinaryOp op varName (replaceValueIf ifValue nextValue value1) (replaceValueIf ifValue nextValue value2)
    UnaryOp op varName value -> UnaryOp op varName (replaceValueIf ifValue nextValue value)
    Phi varName values -> Phi varName (map (\(value, label) -> (replaceValueIf ifValue nextValue value, label)) values)
    If value label1 label2 -> If (replaceValueIf ifValue nextValue value) label1 label2
    Load varName value1 value2 -> Load varName (replaceValueIf ifValue nextValue value1) (replaceValueIf ifValue nextValue value2)
    Store value1 value2 value3 -> Store (replaceValueIf ifValue nextValue value1) (replaceValueIf ifValue nextValue value2) (replaceValueIf ifValue nextValue value3)
    ArrayLength varName value -> ArrayLength varName (replaceValueIf ifValue nextValue value)
    AllocArray varName value -> AllocArray varName (replaceValueIf ifValue nextValue value)
    Get varName value className varName' -> Get varName (replaceValueIf ifValue nextValue value) className varName'
    Set value className varName' value' -> Set (replaceValueIf ifValue nextValue value) className varName' (replaceValueIf ifValue nextValue value')
    AllocObject varName string -> AllocObject varName string
    Assign varName value -> Assign varName (replaceValueIf ifValue nextValue value)
    AssignString varName string -> AssignString varName string
    Call varName functionLabel values -> Call varName functionLabel (map (replaceValueIf ifValue nextValue) values)
    CallMethod varName value className varName' values -> CallMethod varName (replaceValueIf ifValue nextValue value) className varName' (map (replaceValueIf ifValue nextValue) values)
    Goto label -> Goto label
    Return value -> Return (replaceValueIf ifValue nextValue value)
    VReturn -> VReturn
    AddRef varName -> AddRef varName
    RemoveRef varName -> RemoveRef varName
    Self varName -> Self varName

rename :: VarName -> VarName -> VarName -> VarName
rename oldName newName varName = if varName == oldName then newName else varName

renameOutput :: VarName -> VarName -> Statement -> Statement
renameOutput oldName newName = \case
    BinaryOp op varName value1 value2 -> BinaryOp op (rename oldName newName varName) value1 value2
    UnaryOp op varName value -> UnaryOp op (rename oldName newName varName) value
    Phi varName values -> Phi (rename oldName newName varName) values
    If value label1 label2 -> If value label1 label2
    Load varName value1 value2 -> Load (rename oldName newName varName) value1 value2
    Store value1 value2 value3 -> Store value1 value2 value3
    ArrayLength varName value -> ArrayLength (rename oldName newName varName) value
    AllocArray varName value -> AllocArray (rename oldName newName varName) value
    Get varName value className varName' -> Get (rename oldName newName varName) value className varName'
    Set value className varName' value' -> Set value className varName' value'
    AllocObject varName string -> AllocObject (rename oldName newName varName) string
    Assign varName value -> Assign (rename oldName newName varName) value
    AssignString varName string -> AssignString (rename oldName newName varName) string
    Call varName functionLabel values -> Call (fmap (rename oldName newName) varName) functionLabel values
    CallMethod varName value className varName' values -> CallMethod (fmap (rename oldName newName) varName) value className varName' values
    Goto label -> Goto label
    Return value -> Return value
    VReturn -> VReturn
    AddRef varName -> AddRef varName
    RemoveRef varName -> RemoveRef varName
    Self varName -> Self varName

mapValueName :: (VarName -> VarName) -> Value -> Value
mapValueName f = \case
    Variable varName -> Variable (f varName)
    Object varName -> Object (f varName)
    v -> v

mapNames :: (VarName -> VarName) -> Statement -> Statement
mapNames f = \case
    BinaryOp op varName value1 value2 -> BinaryOp op (f varName) (mapValueName f value1) (mapValueName f value2)
    UnaryOp op varName value -> UnaryOp op (f varName) (mapValueName f value)
    Phi varName values -> Phi (f varName) (map (\(value, label) -> (mapValueName f value, label)) values)
    If value label1 label2 -> If (mapValueName f value) label1 label2
    Load varName value1 value2 -> Load (f varName) (mapValueName f value1) (mapValueName f value2)
    Store value1 value2 value3 -> Store (mapValueName f value1) (mapValueName f value2) (mapValueName f value3)
    ArrayLength varName value -> ArrayLength (f varName) (mapValueName f value)
    AllocArray varName value -> AllocArray (f varName) (mapValueName f value)
    Get varName value className varName' -> Get (f varName) (mapValueName f value) className varName'
    Set value className varName' value' -> Set (mapValueName f value) className varName' (mapValueName f value')
    AllocObject varName string -> AllocObject (f varName) string
    Assign varName value -> Assign (f varName) (mapValueName f value)
    AssignString varName string -> AssignString (f varName) string
    Call varName functionLabel values -> Call (fmap f varName) functionLabel (map (mapValueName f) values)
    CallMethod varName value className varName' values -> CallMethod (fmap f varName) (mapValueName f value) className varName' (map (mapValueName f) values)
    Goto label -> Goto label
    Return value -> Return (mapValueName f value)
    VReturn -> VReturn
    AddRef varName -> AddRef (f varName)
    RemoveRef varName -> RemoveRef (f varName)
    Self varName -> Self (f varName)

buildEdges :: ControlGraph -> ControlGraph
buildEdges graph = graph
    { graphEdges = edges
    , graphRevertedEdges = revertedEdges
    }
        where
            edgesFromBlock :: Block -> [Label]
            edgesFromBlock block = case last block of
                Goto label -> [label]
                If _ label1 label2 -> [label1, label2]
                _ -> []

            edges, revertedEdges :: Map.Map Label [Label]
            edges = Map.map edgesFromBlock $ graphData graph
            revertedEdges = Map.fromListWith (++) $ concatMap (\(label, labels) -> map (\label' -> (label', [label])) labels) $ Map.toList edges


userFuncName :: Label -> Label
userFuncName label = "___" ++ label
