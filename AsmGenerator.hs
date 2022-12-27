module AsmGenerator (generateAsmCode) where

import Control.Monad (when)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), modify)
import qualified Data.Map as Map
import qualified Data.Set as Set

import IntermediateTypes (Program, ControlGraph (..), Label, Statement (..), Block, Value (..), BinaryOpType (..), FunctionLabel (..), UnaryOpType (..), VarName)


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

argsRegisters :: [String]
argsRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]


generateAsmCode :: Program -> String
generateAsmCode program = runFunctionBodyGenerator $ do
    emitCmd ".intel_syntax noprefix"
    emitLabel ".LC0"
    emitCmd ".globl main"
    emitEmptyLine
    mapM_ (uncurry generateFunction) $ Map.toAscList program
    emitEmptyLine
    emitCmd ".section .rodata"
    mapM_ (uncurry generateStringLiteral) $ Map.toAscList stringLabels
        where
            stringLabels :: Map.Map String Int
            stringLabels = Map.fromList $ zip (Set.toList $ gatherAllStrings program) [1..]

            generateStringLabel :: String -> String
            generateStringLabel str = ".LC" ++ show (stringLabels Map.! str)

            generateStringLiteral :: String -> Int -> FunctionBodyGenerator ()
            generateStringLiteral str index = do
                emitLabel (".LC" ++ show index)
                emitCmd $ ".string " ++ "\"" ++ escapedString ++ "\\0" ++ "\""
                    where
                        escapedString = reverse $ drop 1 $ reverse $ drop 1 $ show str

            generateFunction :: Label -> ControlGraph -> FunctionBodyGenerator ()
            generateFunction label controlGraph = do
                emitLabel label
                generateFuncBody label controlGraph
                emitCmd "leave"
                emitCmd "ret"
            generateFuncBody :: Label -> ControlGraph -> FunctionBodyGenerator ()
            generateFuncBody funcName controlGraph = do
                emitCmd "push rbp"
                emitCmd "mov rbp, rsp"
                emitCmd $ "sub rsp, " ++ show (8 * allVariablesLength)
                generateArgs $ graphArgs controlGraph
                mapM_ (uncurry generateBlock) (Map.toAscList blocks)
                    where
                        blocks = graphData controlGraph
                        allVariables = gatherAllVariables controlGraph
                        allVariablesLength = length allVariables
                        varIndices = Map.fromList $ zip allVariables [1..]

                        generateJmpLabel :: Label -> Label
                        generateJmpLabel label = funcName ++ "$" ++ label

                        varIndex :: String -> Int
                        varIndex varName = 8 * case Map.lookup varName varIndices of
                            Just index -> index
                            Nothing -> error $ "Variable " ++ varName ++ " not found"

                        varMemory :: String -> String
                        varMemory varName = "QWORD PTR [rbp-" ++ show (varIndex varName) ++ "]"

                        generateArgs :: [VarName] -> FunctionBodyGenerator ()
                        generateArgs args = generateArgs' argsRegisters (length args - length argsRegisters + 1) args
                            where
                                generateArgs' :: [String] -> Int -> [VarName] -> FunctionBodyGenerator ()
                                generateArgs' _ _ [] = return ()
                                generateArgs' (reg:regs) index (arg:as) = do
                                    emitEmptyLine
                                    emitComment $ "arg: " ++ arg
                                    emitCmd $ "mov " ++ varMemory arg ++ ", " ++ reg
                                    generateArgs' regs index as
                                generateArgs' [] index (arg:as) = do
                                    emitEmptyLine
                                    emitComment $ "arg: " ++ arg
                                    emitCmd $ "mov rax" ++ ", QWORD PTR [rbp+" ++ show (8 * index) ++ "]"
                                    emitCmd $ "mov " ++ varMemory arg ++ ", rax"
                                    generateArgs' [] (index - 1) as

                        generateBlock :: Label -> Block -> FunctionBodyGenerator ()
                        generateBlock label block = do
                            emitLabel $ generateJmpLabel label
                            mapM_ generateStatementWithComment block

                        generateValue :: Value -> String
                        generateValue (Variable varName) = varMemory varName
                        generateValue (Object pointer) = varMemory pointer
                        generateValue (Constant int) = show int

                        generateFunctionLabel :: FunctionLabel -> String
                        generateFunctionLabel (FunctionLabel label) = label
                        generateFunctionLabel (Pointer pointer) = show pointer

                        generateCalcBinaryOp :: BinaryOpType -> String
                        generateCalcBinaryOp Add = "add"
                        generateCalcBinaryOp Sub = "sub"
                        generateCalcBinaryOp Mul = "imul"
                        generateCalcBinaryOp _ = error "Not allowed binary operation in this context"

                        generateRelBinaryOp :: BinaryOpType -> String
                        generateRelBinaryOp Equal = "e"
                        generateRelBinaryOp NotEqual = "ne"
                        generateRelBinaryOp Less = "l"
                        generateRelBinaryOp LessEqual = "le"
                        generateRelBinaryOp Greater = "g"
                        generateRelBinaryOp GreaterEqual = "ge"
                        generateRelBinaryOp _ = error "Not allowed binary operation in this context"


                        generateStatementWithComment :: Statement -> FunctionBodyGenerator ()
                        generateStatementWithComment statement = do
                            emitComment $ show statement
                            generateStatement statement
                            emitEmptyLine

                        generateStatement :: Statement -> FunctionBodyGenerator ()
                        generateStatement (Assign varName value) = do
                            emitCmd $ "mov rax, " ++ generateValue value
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (AssignString varName str) = do
                            emitCmd $ "lea rdi, [rip+" ++ generateStringLabel str ++ "]"
                            emitFunctionCall "__copyString" 1
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (Return value) = do
                            emitCmd $ "mov rax, " ++ generateValue value
                            emitCmd "leave"
                            emitCmd "ret"
                        generateStatement VReturn = do
                            emitCmd "leave"
                            emitCmd "ret"
                        generateStatement (BinaryOp Concat varName value1 value2) = do
                            emitCmd $ "mov rdi, " ++ generateValue value1
                            emitCmd $ "mov rsi, " ++ generateValue value2
                            emitFunctionCall "__concat" 2
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (BinaryOp op varName value1 value2) | op `elem` [Add, Sub, Mul] = do
                            emitCmd $ "mov rax, " ++ generateValue value1
                            emitCmd $ "mov rdx, " ++ generateValue value2
                            emitCmd $ generateCalcBinaryOp op ++ " rax, rdx"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (BinaryOp op varName value1 value2) | op `elem` [Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual] = do
                            emitCmd $ "mov rax, " ++ generateValue value1
                            emitCmd $ "cmp rax, " ++ generateValue value2
                            emitCmd "mov rax, 0"
                            emitCmd $ "set" ++ generateRelBinaryOp op ++ " al"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (BinaryOp Div varName value1 value2) = do
                            emitCmd $ "mov rax, " ++ generateValue value1
                            emitCmd $ "mov rcx, " ++ generateValue value2
                            emitCmd "cqo"
                            emitCmd "idiv rcx"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (BinaryOp Mod varName value1 value2) = do
                            emitCmd $ "mov rax, " ++ generateValue value1
                            emitCmd $ "mov rcx, " ++ generateValue value2
                            emitCmd "cqo"
                            emitCmd "idiv rcx"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rdx"
                        generateStatement (UnaryOp Neg varName value) = do
                            emitCmd $ "mov rax, " ++ generateValue value
                            emitCmd "neg rax"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (UnaryOp Not varName value) = do
                            emitCmd $ "mov rax, " ++ generateValue value
                            emitCmd "cmp rax, 0"
                            emitCmd "mov rax, 0"
                            emitCmd "sete al"
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                        generateStatement (Goto label) =
                            emitCmd $ "jmp " ++ generateJmpLabel label
                        generateStatement (If value label1 label2) = do
                            emitCmd $ "mov rax, " ++ generateValue value
                            emitCmd "cmp rax, 0"
                            emitCmd $ "je " ++ generateJmpLabel label2
                            emitCmd $ "jmp " ++ generateJmpLabel label1
                        generateStatement (AddRef varName) = do
                            emitCmd $ "mov rdi, " ++ varMemory varName
                            emitFunctionCall "__addRef" 1
                        generateStatement (RemoveRef varName) = do
                            emitCmd $ "mov rdi, " ++ varMemory varName
                            emitFunctionCall "__removeRef" 1
                        generateStatement (Call varName label values) = do
                            generateFunctionArgs values
                            emitFunctionCall (generateFunctionLabel label) (max (length values - length argsRegisters) 0 + stackAligment)
                            emitCmd $ "mov " ++ varMemory varName ++ ", rax"
                                where
                                    alignStack :: Bool
                                    alignStack = (allVariablesLength + length values) `mod` 2 == 1

                                    stackAligment :: Int
                                    stackAligment = if alignStack then 1 else 0

                                    generateFunctionArgs :: [Value] -> FunctionBodyGenerator ()
                                    generateFunctionArgs = generateFunctionArgs' argsRegisters (allVariablesLength + 1 + stackAligment)
                                        where
                                            generateFunctionArgs' :: [String] -> Int -> [Value] -> FunctionBodyGenerator ()
                                            generateFunctionArgs' _ _ [] = return ()
                                            generateFunctionArgs' [] i (value:vs) = do
                                                emitCmd ("mov rax, " ++ generateValue value)
                                                emitCmd ("mov QWORD PTR [rbp-" ++ show (8 * i) ++ "], rax")
                                                generateFunctionArgs' [] (i + 1) vs
                                            generateFunctionArgs' (reg:regs) i (value:vs) = do
                                                emitCmd ("mov " ++ reg ++ ", " ++ generateValue value)
                                                generateFunctionArgs' regs i vs

                        emitFunctionCall :: String -> Int -> FunctionBodyGenerator ()
                        emitFunctionCall label argsNumber = do
                            when (argsNumber /= 0) $ emitCmd $ "sub rsp, " ++ show (8 * argsNumber)
                            emitCmd $ "call " ++ label
                            when (argsNumber /= 0) $ emitCmd $ "add rsp, " ++ show (8 * argsNumber)



gatherAllStrings :: Program -> Set.Set String
gatherAllStrings = foldr gatherAllStrings' Set.empty . Map.elems
    where
        gatherAllStrings' :: ControlGraph -> Set.Set String -> Set.Set String
        gatherAllStrings' controlGraph acc = foldr gatherAllStrings'' acc $ Map.elems $ graphData controlGraph
        gatherAllStrings'' :: Block -> Set.Set String -> Set.Set String
        gatherAllStrings'' block acc = foldr gatherAllStrings''' acc block
        gatherAllStrings''' :: Statement -> Set.Set String -> Set.Set String
        gatherAllStrings''' (AssignString _ str) = Set.insert str
        gatherAllStrings''' _ = id

gatherAllVariables :: ControlGraph -> [String]
gatherAllVariables graph = Set.toList $ foldr (flip $ foldr gatherVariables) (Set.fromList $ graphArgs graph) $ Map.elems $ graphData graph
    where
        gatherVariables :: Statement -> Set.Set String -> Set.Set String
        gatherVariables (Assign varName _) = Set.insert varName
        gatherVariables (AssignString varName _) = Set.insert varName
        gatherVariables (Load varName _ _) = Set.insert varName
        gatherVariables (BinaryOp _ varName _ _) = Set.insert varName
        gatherVariables (UnaryOp _ varName _) = Set.insert varName
        gatherVariables (Call varName _ _) = Set.insert varName
        gatherVariables _ = id
