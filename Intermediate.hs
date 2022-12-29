{-# LANGUAGE LambdaCase #-}
module Intermediate ( transpile, runIntermediateMonad ) where

import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (local, ask), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Functor ((<&>))
import qualified Data.Map as Map

import qualified Grammar.Abs as Abs

import TypeCheckerTypes ( Type (..), GlobalTypes (..), FuncDef (..) )
import IntermediateTypes ( Program, Label, ControlGraph (..), VarName, Block, Statement (..), Value (..), BinaryOpType (..), UnaryOpType (..), FunctionLabel (..) )

type IntermediateMonad = ExceptT String (ReaderT IEnv IO)

data IEnv = IEnv
    { iEnvTypes :: GlobalTypes
    }

runIntermediateMonad :: GlobalTypes -> IntermediateMonad a -> IO (Either String a)
runIntermediateMonad types monad = runReaderT (runExceptT monad) $ IEnv
    { iEnvTypes = types
    }

transpile :: Abs.Program -> IntermediateMonad Program
transpile (Abs.Program _ defs) = mapM transpileDef defs <&> Map.fromList


transpileDef :: Abs.TopDef -> IntermediateMonad (Label, ControlGraph)
transpileDef (Abs.TopFnDef _ (Abs.FnDef _ _ (Abs.Ident label) args body)) = do
    let argTypes = map (\(Abs.Arg _ t (Abs.Ident argName)) -> (argName, evalType t)) args
    controlGraph <- runControlGraphMonad argTypes $ do
        transpileFuncBody body

        -- if there is no return statement, emit return at the end of function
        emitOnReturnDestruct
        emit VReturn

        pushBlock
        return ()
    return (label, controlGraph)


initialLabel :: Label
initialLabel = "0"

emptyGraph :: [VarName] -> ControlGraph
emptyGraph args = ControlGraph
    { graphData = Map.empty
    , graphEdges = Map.empty
    , graphEntry = initialLabel
    , graphArgs = args
    }


data TranspileStmtFoldData = TranspileStmtFoldData
    { currentLabel :: Label
    , currentBlock :: Block
    , currentGraph :: ControlGraph
    } deriving (Show)

initialTranspileStmtFoldData :: [VarName] -> TranspileStmtFoldData
initialTranspileStmtFoldData args = TranspileStmtFoldData
    { currentLabel = initialLabel
    , currentBlock = []
    , currentGraph = emptyGraph args
    }

data ControlGraphState = ControlGraphState
    { freshVarNames :: [VarName]
    , freshLabels :: [Label]
    , foldData :: TranspileStmtFoldData
    , variablesTypes :: Map.Map VarName Type
    , destructList :: [VarName]
    }

instance Show ControlGraphState where
    show (ControlGraphState _ _ foldData' variablesTypes' _) = "ControlGraphState:\ndata: " ++ show foldData' ++ "\ntypes: " ++ show variablesTypes'

initialControlGraphState :: [(VarName, Type)] -> ControlGraphState
initialControlGraphState argTypes = ControlGraphState
    { freshVarNames = map (\i -> "$t" ++ show i) [1 :: Int ..]
    , freshLabels = map show [1 :: Int ..]
    , foldData = initialTranspileStmtFoldData $ map fst argTypes
    , variablesTypes = Map.fromList argTypes
    , destructList = []
    }

data CEnv = CEnv
    { cEnvVariablesValues :: Map.Map VarName VarName
    , cEnvGlobalTypes :: GlobalTypes
    , cEnvAliveObjects :: [VarName]
    , cEnvBlockObjects :: [VarName]
    }

initialEnv :: [(VarName, Type)] -> GlobalTypes -> CEnv
initialEnv argTypes types = CEnv
    { cEnvVariablesValues = Map.fromList $ map (\(k, _) -> (k, k)) argTypes
    , cEnvGlobalTypes = types
    , cEnvAliveObjects = []
    , cEnvBlockObjects = []
    }

type ControlGraphMonad = ExceptT String (ReaderT CEnv (StateT ControlGraphState IO))

freshTmpNames :: Int -> ControlGraphMonad [VarName]
freshTmpNames k = do
    (varNames, newFreshVarNames) <- gets (splitAt k . freshVarNames)
    modify (\s -> s { freshVarNames = newFreshVarNames })
    return varNames

freshTmpName :: ControlGraphMonad VarName
freshTmpName = do
    tmpNames <- freshTmpNames 1
    if length tmpNames /= 1
        then throwError "TODO XD freshTmpName"
        else return $ head tmpNames

freshLabelsNames:: Int -> ControlGraphMonad [Label]
freshLabelsNames k = do
    (labelsNames, newFreshLabels) <- gets (splitAt k . freshLabels)
    modify (\s -> s { freshLabels = newFreshLabels })
    return labelsNames

freshLabelsName :: ControlGraphMonad Label
freshLabelsName = do
    labelsNames <- freshLabelsNames 1
    if length labelsNames /= 1
        then throwError "TODO XD freshTmpName"
        else return $ head labelsNames


runControlGraphMonad :: [(VarName, Type)] -> ControlGraphMonad () -> IntermediateMonad ControlGraph
runControlGraphMonad argTypes controlGraphMonad = do
    types <- asks iEnvTypes
    (err, state) <- liftIO $ runStateT (runReaderT (runExceptT controlGraphMonad) (initialEnv argTypes types)) $ initialControlGraphState argTypes
    case err of
        Left errString -> throwError errString
        Right _ -> do
            -- liftIO $ print state
            return $ currentGraph $ foldData state

modifyFoldData :: (TranspileStmtFoldData -> TranspileStmtFoldData) -> ControlGraphMonad ()
modifyFoldData f = modify (\s -> s { foldData = f $ foldData s })

emptyLabel :: Label
emptyLabel = ""

pushBlock :: ControlGraphMonad ()
pushBlock = do
    currentLabel' <- gets (currentLabel . foldData)

    when (currentLabel' /= emptyLabel) $
        modifyFoldData (\s -> TranspileStmtFoldData
            { currentLabel = emptyLabel
            , currentBlock = []
            , currentGraph = (currentGraph s) { graphData = Map.insert (currentLabel s) (reverse $ currentBlock s) (graphData $ currentGraph s) }
            })

setLabel :: Label -> ControlGraphMonad ()
setLabel label = modifyFoldData (\s -> s { currentLabel = label })

addEdges :: [(Label, [Label])] -> ControlGraphMonad ()
addEdges edges = modifyFoldData (\s -> s
    { currentGraph = (currentGraph s)
        { graphEdges = foldr (\(k, v) m -> Map.insertWith (++) k v m) (graphEdges $ currentGraph s) edges
        }
    })

newBlock :: Label -> ControlGraphMonad a -> ControlGraphMonad ()
newBlock label prog = do
    pushBlock
    setLabel label
    _ <- prog
    pushBlock

emit :: Statement -> ControlGraphMonad ()
emit stmt = modifyFoldData (\s -> s { currentBlock = stmt : currentBlock s })

evalType :: Abs.Type -> Type
evalType = \case
  Abs.Int _ -> TInt
  Abs.Bool _ -> TBool
  Abs.Str _ -> TString
  Abs.Void _ -> TVoid
  Abs.ClassType _ _ -> undefined
  -- Abs.Array _ (Abs.Array _ _) -> throwException TCMultiDimensionalArray
  Abs.Array _ t -> TArray $ evalType t


foldWithEnv :: (a -> ControlGraphMonad (CEnv -> CEnv)) -> [a] -> ControlGraphMonad (CEnv -> CEnv)
foldWithEnv _ [] = return id
foldWithEnv f (x : xs) = do
    envChange <- f x
    local envChange $ foldWithEnv f xs <&> (. envChange)

emitOnReturnDestruct :: ControlGraphMonad ()
emitOnReturnDestruct = do
    env <- ask
    mapM_ (emit . RemoveRef) $ cEnvBlockObjects env
    mapM_ (emit . RemoveRef) $ cEnvAliveObjects env

addToDestruct :: VarName -> ControlGraphMonad ()
addToDestruct varName = modify (\s -> s { destructList = varName : destructList s })

emitDestruct :: ControlGraphMonad ()
emitDestruct = do
    gets destructList >>= mapM_ (emit . RemoveRef)
    modify (\s -> s { destructList = [] })

transpileFuncBody :: Abs.Block -> ControlGraphMonad ()
transpileFuncBody (Abs.Block _ stmts) = do
    local (\env -> env { cEnvBlockObjects = [], cEnvAliveObjects = cEnvBlockObjects env ++ cEnvAliveObjects env }) $ do
        envChange <- foldWithEnv transpileFuncBodyStmt stmts
        local envChange $ asks cEnvBlockObjects >>= mapM_ (emit . RemoveRef)
        return ()

transpileFuncBodyStmt :: Abs.Stmt -> ControlGraphMonad (CEnv -> CEnv)
transpileFuncBodyStmt stmt = do
    e <- transpileFuncBodyStmt' stmt
    emitDestruct
    return e

emitWhileLoop :: ControlGraphMonad Value -> ControlGraphMonad () -> ControlGraphMonad ()
emitWhileLoop cond body = do
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName

    currentLabel' <- gets $ currentLabel . foldData
    addEdges [(currentLabel', [label1]), (label1, [label2, label3]), (label2, [label1])]

    newBlock label1 $ do
        value <- cond
        emit $ If value label2 label3

    newBlock label2 $ do
        body
        emit $ Goto label1

    setLabel label3

transpileFuncBodyStmt' :: Abs.Stmt -> ControlGraphMonad (CEnv -> CEnv)
transpileFuncBodyStmt' (Abs.SExp _ expr) = transpileFuncBodyExpr expr >> return id
transpileFuncBodyStmt' (Abs.Decl _ t decls) =
    foldWithEnv (transpileFuncBodyDecl t) decls
transpileFuncBodyStmt' (Abs.Ret _ expr) = do
    value <- transpileFuncBodyExpr expr
    emitAddRefIfObject value
    emitOnReturnDestruct
    emit $ Return value
    return id
transpileFuncBodyStmt' (Abs.VRet _) = do
    emitOnReturnDestruct
    emit VReturn
    return id
transpileFuncBodyStmt' (Abs.Empty _) = return id
transpileFuncBodyStmt' (Abs.BStmt _ block) = transpileFuncBody block >> return id
transpileFuncBodyStmt' (Abs.Cond _ expr stmt) = do
    value <- transpileFuncBodyExpr expr
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    emit $ If value label1 label2
    currentLabel' <- gets $ currentLabel . foldData
    addEdges [(currentLabel', [label1, label2]), (label1, [label2])]

    newBlock label1 $ do
        _ <- transpileFuncBodyStmt stmt
        emit $ Goto label2

    setLabel label2
    return id
transpileFuncBodyStmt' (Abs.CondElse _ expr stmt1 stmt2) = do
    value <- transpileFuncBodyExpr expr
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName
    emit $ If value label1 label2
    currentLabel' <- gets $ currentLabel . foldData
    addEdges [(currentLabel', [label1, label2]), (label1, [label3]), (label2, [label3])]

    newBlock label1 $ do
        _ <- transpileFuncBodyStmt stmt1
        emit $ Goto label3

    newBlock label2 $ do
        _ <- transpileFuncBodyStmt stmt2
        emit $ Goto label3

    setLabel label3
    return id
transpileFuncBodyStmt' (Abs.While _ expr stmt) = do
    emitWhileLoop (transpileFuncBodyExpr expr) (void $ transpileFuncBodyStmt stmt)
    return id
transpileFuncBodyStmt' (Abs.Incr _ (Abs.LValue _ expr)) = do
    (varName, value) <- transpileFuncBodyExpr expr <&> \case
        value@(Variable varName) -> (varName, value)
        _ -> error "Incr: not a variable"

    emit $ BinaryOp Add varName value (Constant 1)
    return id
transpileFuncBodyStmt' (Abs.Decr _ (Abs.LValue _ expr)) = do
    (varName, value) <- transpileFuncBodyExpr expr <&> \case
        value@(Variable varName) -> (varName, value)
        _ -> error "Decr: not a variable"

    emit $ BinaryOp Sub varName value (Constant 1)
    return id
transpileFuncBodyStmt' (Abs.Ass _ lvalue expr2) = do
    value <- transpileFuncBodyExpr expr2
    isObject' <- case value of
            Object varName -> do
                emit $ AddRef varName
                return True
            _ -> return False
    transpiledLValue <- transpileLValue lvalue

    let varName = case transpiledLValue of
            LValue varName -> varName
            LValueArrayElem value1 _ -> case value1 of
                Variable varName -> varName
                Object varName -> varName
                _ -> error "LValueArrayElem: not a variable"

    when isObject' $ emit $ RemoveRef varName
    case transpiledLValue of
        LValue varName -> do
            emit $ Assign varName value
        LValueArrayElem value1 value2 ->
            emit $ Store value1 value2 value
    return id
transpileFuncBodyStmt' (Abs.ForLoop _ t' (Abs.Ident varName) (Abs.Ident array) expr) = do
    iteratorVar <- freshTmpName
    addNewVariable iteratorVar TInt
    arrayLength <- freshTmpName
    addNewVariable arrayLength TInt
    comparisonVar <- freshTmpName
    addNewVariable arrayLength TBool

    let t = evalType t'
    tmpVar <- freshTmpName
    addNewVariable tmpVar t

    emit $ Assign iteratorVar (Constant 0)
    emit $ ArrayLength arrayLength (Object array)

    let cond = do
            emit $ BinaryOp Less comparisonVar (Variable iteratorVar) (Variable arrayLength)
            return $ Variable comparisonVar
        body = do
            emit $ Load tmpVar (Object array) (Variable iteratorVar)
            envChange <- transpileDecl t' varName (Variable tmpVar)
            local envChange $ do
                transpileFuncBodyStmt expr
                emit $ BinaryOp Add iteratorVar (Variable iteratorVar) (Constant 1)

    emitWhileLoop cond body
    return id


addNewVariable :: VarName -> Type -> ControlGraphMonad ()
addNewVariable varName t = modify (\s -> s { variablesTypes = Map.insert varName t (variablesTypes s) })

setType :: VarName -> Abs.Type -> ControlGraphMonad (CEnv -> CEnv, VarName)
setType varName t = do
    let t' = evalType t
    cEnvVariablesValues' <- asks cEnvVariablesValues
    let newVarName = case Map.lookup varName cEnvVariablesValues' of
            Nothing -> varName
            Just varName' -> varName' ++ "$"

    addNewVariable newVarName t'
    return (\env -> env { cEnvVariablesValues = Map.insert varName newVarName (cEnvVariablesValues env) }, newVarName)


getTypeFromValue :: Value -> ControlGraphMonad Type
getTypeFromValue (Constant _) = return TInt
getTypeFromValue (Variable varName) = do
    variablesTypes' <- gets variablesTypes
    case Map.lookup varName variablesTypes' of
        Nothing -> error $ "Variable " ++ varName ++ " not found (Variable)"
        Just t -> return t
getTypeFromValue (Object varName) = do
    variablesTypes' <- gets variablesTypes
    case Map.lookup varName variablesTypes' of
        Nothing -> error $ "Variable " ++ varName ++ " not found (Object)"
        Just t -> return t


isObject :: Type -> Bool
isObject TString = True
isObject (TArray _) = True
isObject _ = False


emitAddRefIfObject :: Value -> ControlGraphMonad ()
emitAddRefIfObject = \case
    Object varName -> emit $ AddRef varName
    _ -> return ()


transpileDecl :: Abs.Type -> VarName -> Value -> ControlGraphMonad (CEnv -> CEnv)
transpileDecl t varName value = do
    (envChange, varName') <- setType varName t
    emit $ Assign varName' value
    if isObject $ evalType t
        then do
            emit $ AddRef varName'
            return $ envChange . (\env -> env { cEnvBlockObjects =  varName' : cEnvBlockObjects env})
        else
            return envChange

transpileFuncBodyDecl :: Abs.Type -> Abs.Item -> ControlGraphMonad (CEnv -> CEnv)
transpileFuncBodyDecl t (Abs.NoInit _ (Abs.Ident varName)) = transpileDecl t varName (Constant 0)
transpileFuncBodyDecl t (Abs.Init _ (Abs.Ident varName) expr) = transpileFuncBodyExpr expr >>= transpileDecl t varName


transpileAddOp :: Abs.AddOp -> BinaryOpType
transpileAddOp = \case
    Abs.Plus _ -> Add
    Abs.Minus _ -> Sub

transpileMulOp :: Abs.MulOp -> BinaryOpType
transpileMulOp = \case
    Abs.Times _ -> Mul
    Abs.Div _ -> Div
    Abs.Mod _ -> Mod

transpileRelOp :: Abs.RelOp -> BinaryOpType
transpileRelOp = \case
    Abs.LTH _ -> Less
    Abs.LE _ -> LessEqual
    Abs.GTH _ -> Greater
    Abs.GE _ -> GreaterEqual
    Abs.EQU _ -> Equal
    Abs.NE _ -> NotEqual

createValue :: VarName -> Type -> ControlGraphMonad Value
createValue varName t =
    gets (Map.lookup varName . variablesTypes) >>= \case
        Nothing -> error $ "Variable " ++ varName ++ " not found"
        Just t -> return $ if isObject t then Object varName else Variable varName

data LValue = LValue VarName
            | LValueArrayElem Value Value

transpileLValue :: Abs.LValue -> ControlGraphMonad LValue
transpileLValue (Abs.LValue _ (Abs.EVar _ (Abs.Ident varName))) = return $ LValue varName
transpileLValue (Abs.LValue _ (Abs.EArrayElem _ expr1 expr2)) = do
    value1 <- transpileFuncBodyExpr expr1
    value2 <- transpileFuncBodyExpr expr2
    return $ LValueArrayElem value1 value2


transpileFuncBodyExpr :: Abs.Expr -> ControlGraphMonad Value
transpileFuncBodyExpr (Abs.ELitInt _ i) = return $ Constant $ fromInteger i
transpileFuncBodyExpr (Abs.ELitTrue _) = return $ Constant 1
transpileFuncBodyExpr (Abs.ELitFalse _) = return $ Constant 0
transpileFuncBodyExpr (Abs.EString _ s) = do
    tmpName <- freshTmpName
    addNewVariable tmpName TString

    emit $ AssignString tmpName s
    return $ Object tmpName
transpileFuncBodyExpr (Abs.EVar _ (Abs.Ident varName)) = do
    gets (Map.lookup varName . variablesTypes) >>= \case
        Nothing -> error $ "Variable " ++ varName ++ " not found"
        Just t -> return $ if isObject t then Object varName else Variable varName
transpileFuncBodyExpr (Abs.EAdd _ expr1 op expr2) = do
    value1 <- transpileFuncBodyExpr expr1
    value2 <- transpileFuncBodyExpr expr2
    t <- getTypeFromValue value1
    case (t, op) of
        (TInt, _) -> do
            tmpName <- freshTmpName
            addNewVariable tmpName TInt

            emit $ BinaryOp (transpileAddOp op) tmpName value1 value2
            return $ Variable tmpName
        (TString, Abs.Plus _) -> do
            tmpName <- freshTmpName
            addNewVariable tmpName TString

            emit $ BinaryOp Concat tmpName value1 value2
            return $ Object tmpName
        _ -> error "Invalid types for addition"
transpileFuncBodyExpr (Abs.EMul _ expr1 op expr2) = do
    value1 <- transpileFuncBodyExpr expr1
    value2 <- transpileFuncBodyExpr expr2

    tmpName <- freshTmpName
    addNewVariable tmpName TInt

    emit $ BinaryOp (transpileMulOp op) tmpName value1 value2
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.ERel _ expr1 op expr2) = do
    value1 <- transpileFuncBodyExpr expr1
    value2 <- transpileFuncBodyExpr expr2

    tmpName <- freshTmpName
    addNewVariable tmpName TBool

    emit $ BinaryOp (transpileRelOp op) tmpName value1 value2
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.EApp _ (Abs.Ident funcName) exprs) = do
    values <- mapM transpileFuncBodyExpr exprs

    -- TODO: methods
    tmpName <- freshTmpName
    funcReturnType' <- asks $ funcReturnType . (Map.! funcName) . globalFunctions . cEnvGlobalTypes
    addNewVariable tmpName funcReturnType'

    mapM_ (\case
        Object varName -> do
            emit $ AddRef varName
            addToDestruct varName
        _ -> return ()) values
    emit $ Call tmpName (FunctionLabel funcName) values
    when (isObject funcReturnType') $ addToDestruct tmpName

    return $ if isObject funcReturnType' then Object tmpName else Variable tmpName
transpileFuncBodyExpr (Abs.EOr _ expr1 expr2) = do
    value1 <- transpileFuncBodyExpr expr1
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName
    emit $ If value1 label1 label2

    tmpName <- freshTmpName
    addNewVariable tmpName TBool

    currentLabel' <- gets $ currentLabel . foldData
    addEdges [(currentLabel', [label1, label2]), (label1, [label3]), (label2, [label3])]

    newBlock label1 $ do
        emit $ Assign tmpName (Constant 1)
        emit $ Goto label3

    newBlock label2 $ do
        value2 <- transpileFuncBodyExpr expr2
        emit $ Assign tmpName value2
        emit $ Goto label3

    setLabel label3
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.EAnd _ expr1 expr2) = do
    value1 <- transpileFuncBodyExpr expr1
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName
    emit $ If value1 label1 label2

    tmpName <- freshTmpName
    addNewVariable tmpName TBool

    currentLabel' <- gets $ currentLabel . foldData
    addEdges [(currentLabel', [label1, label2]), (label1, [label3]), (label2, [label3])]

    newBlock label1 $ do
        value2 <- transpileFuncBodyExpr expr2
        emit $ Assign tmpName value2
        emit $ Goto label3

    newBlock label2 $ do
        emit $ Assign tmpName (Constant 1)
        emit $ Goto label3

    setLabel label3
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.Neg _ expr) = do
    value <- transpileFuncBodyExpr expr
    tmpName <- freshTmpName
    addNewVariable tmpName TInt

    emit $ UnaryOp Neg tmpName value
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.Not _ expr) = do
    value <- transpileFuncBodyExpr expr
    tmpName <- freshTmpName
    addNewVariable tmpName TBool

    emit $ UnaryOp Not tmpName value
    return $ Variable tmpName
transpileFuncBodyExpr (Abs.EAlloc _ t expr) = do
    value <- transpileFuncBodyExpr expr
    tmpName <- freshTmpName
    addNewVariable tmpName (TArray $ evalType t)

    emit $ AllocArray tmpName value
    return $ Object tmpName
transpileFuncBodyExpr (Abs.EArrayElem _ expr1 expr2) = do
    array <- transpileFuncBodyExpr expr1
    value <- transpileFuncBodyExpr expr2

    t <- getTypeFromValue array >>= \case
        TArray t' -> return t'
        _ -> error "transpileFuncBodyExpr(Abs.EArrayElem): Array expected"

    tmpName <- freshTmpName
    addNewVariable tmpName t

    emit $ Load tmpName array value
    createValue tmpName t
transpileFuncBodyExpr (Abs.EAttr _ expr1 (Abs.Ident attr)) = do
    object <- transpileFuncBodyExpr expr1

    case attr of
        "length" -> do
            tmpName <- freshTmpName
            addNewVariable tmpName TInt

            emit $ ArrayLength tmpName object
            return $ Variable tmpName
        _ -> undefined --TODO: attributes
