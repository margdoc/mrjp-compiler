module AsmGenerator (generateAsmCode) where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), modify)
import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (graphData), Label, Statement (..), Block, Value (..), BinaryOpType (..))


generateCmd :: String -> String
generateCmd c = '\t' : c ++ "\n"

generateLabel :: String -> String
generateLabel l = l ++ ":\n"

generateComment :: String -> String
generateComment c = "\t// " ++ c ++ "\n"


data AsmLine = Cmd String
             | Label String
             | Comment String
             | EmptyLine


generateAsmLines :: [AsmLine] -> String
generateAsmLines = concatMap generateAsmLine
    where
        generateAsmLine :: AsmLine -> String
        generateAsmLine (Cmd cmd) = generateCmd cmd
        generateAsmLine (Label label) = generateLabel label
        generateAsmLine (Comment comment) = generateComment comment
        generateAsmLine EmptyLine = "\n"


data Env = Env {}

initialEnv :: Env
initialEnv = Env {}

data State = State
    { asmLines :: [AsmLine]
    }

initialState :: State
initialState = State
    { asmLines = []
    }

type FunctionBodyGenerator = ExceptT String (ReaderT Env (StateT State Identity))

runFunctionBodyGenerator :: FunctionBodyGenerator a -> String
runFunctionBodyGenerator generator = case fst output of
        Left err -> error err
        Right _ -> generateAsmLines $ reverse $ asmLines $ snd output
    where
        output = runIdentity $ runStateT (runReaderT (runExceptT generator) initialEnv) initialState

emitAsmLine :: AsmLine -> FunctionBodyGenerator ()
emitAsmLine asmLine = modify $ \s -> s { asmLines = asmLine : asmLines s }

emitCmd :: String -> FunctionBodyGenerator ()
emitCmd = emitAsmLine . Cmd

emitLabel :: String -> FunctionBodyGenerator ()
emitLabel = emitAsmLine . Label

emitComment :: String -> FunctionBodyGenerator ()
emitComment = emitAsmLine . Comment

emitEmptyLine :: FunctionBodyGenerator ()
emitEmptyLine = emitAsmLine EmptyLine

generateFuncBody :: ControlGraph -> FunctionBodyGenerator ()
generateFuncBody controlGraph = do
    emitCmd "push rbp"
    emitCmd "mov rbp, rsp"
    emitCmd ("sub rsp, " ++ show (8 * length allVariables))
    mapM_ (uncurry generateBlock) (Map.toAscList blocks)
    where
        blocks = graphData controlGraph
        allVariables = gatherAllVariables blocks
        varIndices = Map.fromList $ zip allVariables [1..]

        varIndex :: String -> Int
        varIndex varName = varIndices Map.! varName * 8

        varMemory :: String -> String
        varMemory varName = "QWORD PTR [rbp-" ++ show (varIndex varName) ++ "]"

        generateBlock :: Label -> Block -> FunctionBodyGenerator ()
        generateBlock label block = do
            emitLabel label
            mapM_ generateStatementWithComment block

        generateValue :: Value -> String
        generateValue (Variable varName) = varMemory varName
        generateValue (Constant int) = show int

        generateBinaryOp :: BinaryOpType -> String
        generateBinaryOp Add = "add"
        generateBinaryOp Sub = "sub"
        generateBinaryOp Mul = "imul"
        generateBinaryOp Div = "idiv"

        generateStatementWithComment :: Statement -> FunctionBodyGenerator ()
        generateStatementWithComment statement = do
            emitComment (show statement)
            generateStatement statement
            emitEmptyLine

        generateStatement :: Statement -> FunctionBodyGenerator ()
        generateStatement (Assign varName value) = do
            emitCmd ("mov rax, " ++ generateValue value)
            emitCmd ("mov " ++ varMemory varName ++ ", rax")
        generateStatement (Return value) = do
            emitCmd ("mov rax, " ++ generateValue value)
            emitCmd "leave"
            emitCmd "ret"
        generateStatement (BinaryOp op varName value1 value2) = do
            emitCmd ("mov rax, " ++ generateValue value1)
            emitCmd ("mov rdx, " ++ generateValue value2)
            emitCmd (generateBinaryOp op ++ " rax, rdx")
            emitCmd ("mov " ++ varMemory varName ++ ", rax")
        generateStatement (Goto label) =
            emitCmd ("jmp " ++ label)
        generateStatement (If value label1 label2) = do
            emitCmd ("cmp " ++ generateValue value ++ ", 0")
            emitCmd ("je " ++ label2)
            emitCmd ("jmp " ++ label1)


gatherAllVariables :: Map.Map Label Block -> [String]
gatherAllVariables blocks = Set.toList $ foldr (flip $ foldr gatherVariables) Set.empty $ Map.elems blocks
    where
        gatherVariables :: Statement -> Set.Set String -> Set.Set String
        gatherVariables (Assign varName _) = Set.insert varName
        gatherVariables (Load varName _ _) = Set.insert varName
        gatherVariables (BinaryOp _ varName _ _) = Set.insert varName
        gatherVariables (UnaryOp _ varName _) = Set.insert varName
        gatherVariables _ = id


generateFunction :: Label -> ControlGraph -> FunctionBodyGenerator ()
generateFunction label controlGraph = do
    emitLabel label
    generateFuncBody controlGraph

generateAsmCode :: Program -> String
generateAsmCode program = runFunctionBodyGenerator $ do
    emitCmd ".intel_syntax noprefix"
    emitLabel ".LC0"
    emitCmd ".globl main"
    emitEmptyLine
    mapM_ (uncurry generateFunction) $ Map.toAscList program
