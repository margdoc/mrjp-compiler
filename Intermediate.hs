{-# LANGUAGE LambdaCase #-}
module Intermediate ( transpile, runIntermediateMonad ) where

import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (local, ask), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Functor ((<&>))
import qualified Data.Map as Map

import qualified Grammar.Abs as Abs

import TypeCheckerTypes ( Type (..), GlobalTypes (..), FuncDef (..), ClassDef (..) )
import IntermediateTypes ( Program, Label, ControlGraph (..), VarName, Block, Statement (..), Value (..), BinaryOpType (..), UnaryOpType (..), methodLabel, buildEdges, userFuncName )
import TypeChecker (selfKeyword)

type IntermediateMonad = ExceptT String (ReaderT IEnv IO)

data IEnv = IEnv
    { iEnvTypes :: GlobalTypes
    }

runIntermediateMonad :: GlobalTypes -> IntermediateMonad a -> IO (Either String a)
runIntermediateMonad types monad = runReaderT (runExceptT monad) $ IEnv
    { iEnvTypes = types
    }

transpile :: Abs.Program -> IntermediateMonad Program
transpile (Abs.Program _ defs) = mapM transpileDef defs <&> Map.fromList . concat

transpileFuncBody :: Abs.Block -> [(VarName, Type)] -> IntermediateMonad ControlGraph
transpileFuncBody body argTypes = runControlGraphMonad argTypes $ do
    transpileBlock body

    -- if there is no return statement, emit return at the end of function
    emitOnReturnDestruct
    emit VReturn

    pushBlock
    return ()

transpileMethodDef :: VarName -> Abs.FnDef -> IntermediateMonad (Label, ControlGraph)
transpileMethodDef className (Abs.FnDef _ _ (Abs.Ident label) args body) = do
    let argTypes = map (\(Abs.Arg _ t (Abs.Ident argName)) -> (argName, evalType t)) args
    controlGraph <- transpileFuncBody body ((selfKeyword, TClass className) : argTypes)
    return (userFuncName $ methodLabel className label, controlGraph)

transpileDef :: Abs.TopDef -> IntermediateMonad [(Label, ControlGraph)]
transpileDef (Abs.TopFnDef _ (Abs.FnDef _ _ (Abs.Ident label) args body)) = do
    let argTypes = map (\(Abs.Arg _ t (Abs.Ident argName)) -> (argName, evalType t)) args
    controlGraph <- transpileFuncBody body argTypes
    return $ return (userFuncName label, buildEdges controlGraph)
transpileDef (Abs.TopClassDef _ classTopDef) = do
    classDef <- asks (Map.lookup className . globalClasses . iEnvTypes) <&> \case
        Just classDef -> classDef
        Nothing -> error $ "transpileDef(Abs.TopClassDef): class " ++ className ++ " not found"
    local (\env -> env { iEnvTypes = (iEnvTypes env)
        { globalFunctions = Map.union (classMethods classDef) (globalFunctions $ iEnvTypes env)
        } }) $ mapM (transpileMethodDef className) classMethodsDefs
        where
            className = case classTopDef of
                Abs.ClassDefSimple _ (Abs.Ident name) _ -> name
                Abs.ClassDefExtended _ (Abs.Ident name) _ _ -> name
            classDefs = case classTopDef of
                Abs.ClassDefSimple _ _ methods' -> methods'
                Abs.ClassDefExtended _ _ _ methods' -> methods'
            classMethodsDefs = map (\case Abs.ClassFnDef _ fnDef -> fnDef; _ -> error "transpileDef(Abs.TopClassDef): not Abs.ClassFnDef") $
                        filter (\case Abs.ClassFnDef{} -> True; _ -> False) classDefs


initialLabel :: Label
initialLabel = "0"

emptyGraph :: [VarName] -> ControlGraph
emptyGraph args = ControlGraph
    { graphData = Map.empty
    , graphEdges = Map.empty
    , graphRevertedEdges = Map.empty
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
    { cEnvVariablesValues = Map.fromList (map (\(k, _) -> (k, k)) argTypes)
    , cEnvGlobalTypes = types
    , cEnvAliveObjects = []
    , cEnvBlockObjects = []
    }

type ControlGraphMonad = ExceptT String (ReaderT CEnv (StateT ControlGraphState IO))


freshTmpName :: ControlGraphMonad VarName
freshTmpName = do
    (varName : newFreshVarNames) <- gets freshVarNames
    modify (\s -> s { freshVarNames = newFreshVarNames })
    return varName

freshLabelsName :: ControlGraphMonad Label
freshLabelsName = do
    (labelName : newFreshLabels) <- gets freshLabels
    modify (\s -> s { freshLabels = newFreshLabels })
    return labelName


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
  Abs.ClassType _ (Abs.Ident t) -> TClass t
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

transpileBlock :: Abs.Block -> ControlGraphMonad ()
transpileBlock (Abs.Block _ stmts) = do
    local (\env -> env { cEnvBlockObjects = [], cEnvAliveObjects = cEnvBlockObjects env ++ cEnvAliveObjects env }) $ do
        envChange <- foldWithEnv transpileStmt stmts
        local envChange $ asks cEnvBlockObjects >>= mapM_ (emit . RemoveRef)
        return ()

transpileStmt :: Abs.Stmt -> ControlGraphMonad (CEnv -> CEnv)
transpileStmt stmt = do
    e <- transpileStmt' stmt
    emitDestruct
    return e

emitWhileLoop :: (Label -> Label -> ControlGraphMonad ()) -> ControlGraphMonad () -> ControlGraphMonad ()
emitWhileLoop cond body = do
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName

    emit $ Goto label1

    newBlock label1 $ cond label2 label3

    newBlock label2 $ do
        body
        emit $ Goto label1

    setLabel label3

transpileStmt' :: Abs.Stmt -> ControlGraphMonad (CEnv -> CEnv)
transpileStmt' (Abs.SExp _ expr) = transpileExpr expr >> return id
transpileStmt' (Abs.Decl _ t decls) =
    foldWithEnv (transpileDecl t) decls
transpileStmt' (Abs.Ret _ expr) = do
    value <- transpileExpr expr
    emitAddRefIfObject value
    emitOnReturnDestruct
    emit $ Return value
    return id
transpileStmt' (Abs.VRet _) = do
    emitOnReturnDestruct
    emit VReturn
    return id
transpileStmt' (Abs.Empty _) = return id
transpileStmt' (Abs.BStmt _ block) = transpileBlock block >> return id
transpileStmt' (Abs.Cond _ expr stmt) = do
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    transpileCondition expr label1 label2

    newBlock label1 $ do
        _ <- transpileStmt stmt
        emit $ Goto label2

    setLabel label2
    return id
transpileStmt' (Abs.CondElse _ expr stmt1 stmt2) = do
    label1 <- freshLabelsName
    label2 <- freshLabelsName
    label3 <- freshLabelsName
    transpileCondition expr label1 label2

    newBlock label1 $ do
        _ <- transpileStmt stmt1
        emit $ Goto label3

    newBlock label2 $ do
        _ <- transpileStmt stmt2
        emit $ Goto label3

    setLabel label3
    return id
transpileStmt' (Abs.While _ expr stmt) = do
    emitWhileLoop (transpileCondition expr) (void $ transpileStmt stmt)
    return id
transpileStmt' (Abs.Incr _ (Abs.LValue _ expr)) = do
    (varName, value) <- transpileExpr expr <&> \case
        value@(Variable varName) -> (varName, value)
        _ -> error "Incr: not a variable"

    tmpName <- getNewVariable TInt
    emit $ BinaryOp Add tmpName value (Constant 1)
    emit $ Assign varName (Variable tmpName)
    return id
transpileStmt' (Abs.Decr _ (Abs.LValue _ expr)) = do
    (varName, value) <- transpileExpr expr <&> \case
        value@(Variable varName) -> (varName, value)
        _ -> error "Decr: not a variable"

    tmpName <- getNewVariable TInt
    emit $ BinaryOp Sub tmpName value (Constant 1)
    emit $ Assign varName (Variable tmpName)
    return id
transpileStmt' (Abs.Ass _ lvalue expr2) = do
    value <- transpileExpr expr2
    isObject' <- case value of
        Object varName -> do
            emit $ AddRef varName
            return True
        _ -> return False
    transpiledLValue <- transpileLValue lvalue

    let removeRef getStatement = do
        t <- getTypeFromValue value
        tmpName <- getNewVariable t
        emit $ getStatement tmpName
        emit $ RemoveRef tmpName

    case transpiledLValue of
        LValue varName -> do
            when isObject' $ emit $ RemoveRef varName
            emit $ Assign varName value
        LValueArrayElem value1 value2 -> do
            when isObject' $ removeRef (\tmp -> Load tmp value1 value2)
            emit $ Store value1 value2 value
        LValueAttr value1 attrName -> do
            className <- getClassName value1
            when isObject' $ removeRef (\tmp -> Get tmp value1 className attrName)
            emit $ Set value1 className attrName value
    return id
transpileStmt' (Abs.ForLoop _ t' (Abs.Ident varName) (Abs.Ident array) expr) = do
    iteratorVar <- getNewVariable TInt
    arrayLength <- getNewVariable TInt
    comparisonVar <- getNewVariable TBool

    let t = evalType t'
    tmpVar <- getNewVariable t

    emit $ Assign iteratorVar (Constant 0)
    emit $ ArrayLength arrayLength (Object array)

    let cond trueLabel falseLabel = do
            emit $ BinaryOp Less comparisonVar (Variable iteratorVar) (Variable arrayLength)
            emit $ If (Variable comparisonVar) trueLabel falseLabel
        body = do
            emit $ Load tmpVar (Object array) (Variable iteratorVar)
            envChange <- transpileDecl' t' varName (Variable tmpVar)
            local envChange $ do
                _ <- transpileStmt expr
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
            Just varName' -> varName' ++ "#"

    addNewVariable newVarName t'
    return (\env -> env { cEnvVariablesValues = Map.insert varName newVarName cEnvVariablesValues' }, newVarName)

getClassName :: Value -> ControlGraphMonad VarName
getClassName value = getTypeFromValue value <&> \case
    TClass t -> t
    _ -> error "getClassName: not a class"

getTypeFromValue :: Value -> ControlGraphMonad Type
getTypeFromValue (Constant _) = return TInt
getTypeFromValue (Variable varName) = getVarType varName
getTypeFromValue (Object varName) = getVarType varName


isObject :: Type -> Bool
isObject TString = True
isObject (TArray _) = True
isObject (TClass _) = True
isObject _ = False


emitAddRefIfObject :: Value -> ControlGraphMonad ()
emitAddRefIfObject = \case
    Object varName -> emit $ AddRef varName
    _ -> return ()


transpileDecl' :: Abs.Type -> VarName -> Value -> ControlGraphMonad (CEnv -> CEnv)
transpileDecl' t varName value = do
    (envChange, varName') <- setType varName t
    case (evalType t, value) of
        (TString, Constant _) -> emit $ AssignString varName' ""
        _ -> emit $ Assign varName' value
    if isObject $ evalType t
        then do
            emit $ AddRef varName'
            return $ envChange . (\env -> env { cEnvBlockObjects =  varName' : cEnvBlockObjects env})
        else
            return envChange

transpileDecl :: Abs.Type -> Abs.Item -> ControlGraphMonad (CEnv -> CEnv)
transpileDecl t (Abs.NoInit _ (Abs.Ident varName)) = transpileDecl' t varName (Constant 0)
transpileDecl t (Abs.Init _ (Abs.Ident varName) expr) = transpileExpr expr >>= transpileDecl' t varName


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

createValue :: VarName -> ControlGraphMonad Value
createValue varName = getVarType varName <&> \t -> if isObject t then Object varName else Variable varName

isAttr :: VarName -> ControlGraphMonad (Maybe Type)
isAttr varName = do
    gets (Map.lookup varName . variablesTypes) >>= \case
        Just _ -> return Nothing
        _ ->  gets (Map.lookup selfKeyword . variablesTypes) >>= \case
            Just _ -> do
                className <- getVarType selfKeyword >>= \case
                    TClass t -> return t
                    _ -> error "transpileLValue(Abs.LValue): Class expected"

                asks (Map.lookup className . globalClasses . cEnvGlobalTypes) >>= \case
                    Just classDef -> return $ Map.lookup varName $ classAttributes classDef
                    _ -> error "transpileLValue(Abs.LValue): Class not found"
            _ -> return Nothing

data LValue = LValue VarName
            | LValueArrayElem Value Value
            | LValueAttr Value VarName

transpileLValue :: Abs.LValue -> ControlGraphMonad LValue
transpileLValue (Abs.LValue _ (Abs.EVar _ (Abs.Ident varName))) =
    isAttr varName >>= \case
        Just _ -> do
            self <- getSelf
            return $ LValueAttr self varName
        _ -> currentVarName varName <&> LValue
transpileLValue (Abs.LValue _ (Abs.EArrayElem _ expr1 expr2)) = do
    value1 <- transpileExpr expr1
    value2 <- transpileExpr expr2
    return $ LValueArrayElem value1 value2
transpileLValue (Abs.LValue _ (Abs.EAttr _ expr (Abs.Ident attrName))) = do
    value <- transpileExpr expr
    return $ LValueAttr value attrName
transpileLValue _ = error "transpileLValue: Invalid LValue"

getVarType :: VarName -> ControlGraphMonad Type
getVarType varName = gets (Map.lookup varName . variablesTypes) >>= \case
    Nothing -> error $ "Variable " ++ varName ++ " not found"
    Just t -> return t

getSelf :: ControlGraphMonad Value
getSelf = do
    getVarType selfKeyword >>= \t -> do
        tmpName <- getNewVariable t
        emit $ Assign tmpName (Object selfKeyword)
        return $ Object tmpName

getNewVariable :: Type -> ControlGraphMonad VarName
getNewVariable t = do
    tmpName <- freshTmpName
    addNewVariable tmpName t
    return tmpName

transpileMethodApp :: Value -> Label -> [Value] -> ControlGraphMonad Value
transpileMethodApp object funcName args = do
    className <- getClassName object
    funcReturnType' <- asks (Map.lookup className . globalClasses . cEnvGlobalTypes) >>= \case
        Just classDef -> case Map.lookup funcName $ classMethods classDef of
            Just t' -> return $ funcReturnType t'
            _ -> error "transpileMethodApp: Method not found"
        _ -> error "transpileMethodApp: Class not found"
    transpileFuncApp (\tmp -> CallMethod tmp object className) funcReturnType' funcName args

transpileFuncApp :: (Maybe VarName -> Label -> [Value] -> Statement) -> Type -> Label -> [Value] -> ControlGraphMonad Value
transpileFuncApp funcCtor t funcName args = do
    tmp <- case t of
        TVoid -> return Nothing
        _ -> getNewVariable t <&> Just

    mapM_ (\case
        Object varName@('$':_) -> do -- only for temporary variables
            emit $ AddRef varName
            addToDestruct varName
        _ -> return ()) args

    emit $ funcCtor tmp (userFuncName funcName) args

    case tmp of
        Nothing -> return $ Constant 0
        Just tmpName -> do
            when (isObject t) $ addToDestruct tmpName
            createValue tmpName

transpileCondition :: Abs.Expr -> Label -> Label -> ControlGraphMonad ()
transpileCondition (Abs.ELitTrue _) trueLabel _ = emit $ Goto trueLabel
transpileCondition (Abs.ELitFalse _) _ falseLabel = emit $ Goto falseLabel
transpileCondition (Abs.Not _ expr) trueLabel falseLabel = transpileCondition expr falseLabel trueLabel
transpileCondition (Abs.EAnd _ expr1 expr2) trueLabel falseLabel = do
    middleLabel <- freshLabelsName
    transpileCondition expr1 middleLabel falseLabel
    newBlock middleLabel $ transpileCondition expr2 trueLabel falseLabel
transpileCondition (Abs.EOr _ expr1 expr2) trueLabel falseLabel = do
    middleLabel <- freshLabelsName
    transpileCondition expr1 trueLabel middleLabel
    newBlock middleLabel $ transpileCondition expr2 trueLabel falseLabel
transpileCondition expr trueLabel falseLabel = do
    value <- transpileExpr expr
    emit $ If value trueLabel falseLabel

currentVarName :: VarName -> ControlGraphMonad VarName
currentVarName varName = asks $ (Map.! varName) . cEnvVariablesValues

transpileExpr :: Abs.Expr -> ControlGraphMonad Value
transpileExpr (Abs.ELitInt _ i) = return $ Constant $ fromInteger i
transpileExpr (Abs.ELitTrue _) = return $ Constant 1
transpileExpr (Abs.ELitFalse _) = return $ Constant 0
transpileExpr (Abs.EString _ s) = do
    tmpName <- getNewVariable TString

    emit $ AssignString tmpName s
    return $ Object tmpName
transpileExpr (Abs.EVar _ (Abs.Ident varName)) =
    isAttr varName >>= \case
        Just t -> do
            tmpName <- getNewVariable t
            self <- getSelf
            className <- getClassName self
            emit $ Get tmpName self className varName
            createValue tmpName
        Nothing -> currentVarName varName >>= createValue
transpileExpr (Abs.EAdd _ expr1 op expr2) = do
    value1 <- transpileExpr expr1
    value2 <- transpileExpr expr2
    t <- getTypeFromValue value1
    case (t, op) of
        (TInt, _) -> do
            tmpName <- getNewVariable TInt

            emit $ BinaryOp (transpileAddOp op) tmpName value1 value2
            return $ Variable tmpName
        (TString, Abs.Plus _) -> do
            tmpName <- getNewVariable TString

            emit $ BinaryOp Concat tmpName value1 value2
            return $ Object tmpName
        _ -> error "Invalid types for addition"
transpileExpr (Abs.EMul _ expr1 op expr2) = do
    value1 <- transpileExpr expr1
    value2 <- transpileExpr expr2

    tmpName <- getNewVariable TInt

    emit $ BinaryOp (transpileMulOp op) tmpName value1 value2
    return $ Variable tmpName
transpileExpr (Abs.ERel _ expr1 op expr2) = do
    value1 <- transpileExpr expr1
    value2 <- transpileExpr expr2

    tmpName <- getNewVariable TBool
    t <- getTypeFromValue value1

    case (t, op) of
        (TString, Abs.EQU _) -> emit $ BinaryOp StringEqual tmpName value1 value2
        (TString, Abs.NE _) -> emit $ BinaryOp StringNotEqual tmpName value1 value2
        (TClass _, Abs.EQU _) -> emit $ BinaryOp Equal tmpName value1 value2
        (TClass _, Abs.NE _) -> emit $ BinaryOp NotEqual tmpName value1 value2
        (TArray _, Abs.EQU _) -> emit $ BinaryOp Equal tmpName value1 value2
        (TArray _, Abs.NE _) -> emit $ BinaryOp NotEqual tmpName value1 value2
        (TString, _) -> error "Invalid types for relation"
        (TClass _, _) -> error "Invalid types for relation"
        (TArray _, _) -> error "Invalid types for relation"
        (_, _) -> emit $ BinaryOp (transpileRelOp op) tmpName value1 value2

    return $ Variable tmpName
transpileExpr (Abs.EApp _ (Abs.Ident funcName) exprs) = do
    values <- mapM transpileExpr exprs

    let emitFunc = do
        funcReturnType' <- asks $ funcReturnType . (Map.! funcName) . globalFunctions . cEnvGlobalTypes
        transpileFuncApp Call funcReturnType' funcName values

    gets (Map.lookup selfKeyword . variablesTypes) >>= \case
        Just _ -> do
            className <- getVarType selfKeyword >>= \case
                TClass t -> return t
                _ -> error "transpileExpr(Abs.EApp): Class expected"

            asks (Map.lookup className . globalClasses . cEnvGlobalTypes) >>= \case
                Just classDef -> case Map.lookup funcName $ classMethods classDef of
                    Just _ -> do
                        self <- getSelf
                        transpileMethodApp self funcName values
                    _ -> emitFunc
                _ -> error "transpileExpr(Abs.EApp): Class not found"
        _ -> emitFunc

transpileExpr cond@Abs.EOr{} = do
    trueLabel <- freshLabelsName
    falseLabel <- freshLabelsName
    nextLabel <- freshLabelsName

    transpileCondition cond trueLabel falseLabel

    tmpName <- getNewVariable TBool
    newBlock trueLabel $ do
        emit $ Assign tmpName (Constant 1)
        emit $ Goto nextLabel
    newBlock falseLabel $ do
        emit $ Assign tmpName (Constant 0)
        emit $ Goto nextLabel

    setLabel nextLabel
    return $ Variable tmpName
transpileExpr cond@Abs.EAnd{} = do
    trueLabel <- freshLabelsName
    falseLabel <- freshLabelsName
    nextLabel <- freshLabelsName

    transpileCondition cond trueLabel falseLabel

    tmpName <- getNewVariable TBool
    newBlock trueLabel $ do
        emit $ Assign tmpName (Constant 1)
        emit $ Goto nextLabel
    newBlock falseLabel $ do
        emit $ Assign tmpName (Constant 0)
        emit $ Goto nextLabel

    setLabel nextLabel
    return $ Variable tmpName
transpileExpr (Abs.Neg _ expr) = do
    value <- transpileExpr expr
    tmpName <- getNewVariable TInt

    emit $ UnaryOp Neg tmpName value
    return $ Variable tmpName
transpileExpr (Abs.Not _ expr) = do
    value <- transpileExpr expr
    tmpName <- getNewVariable TBool

    emit $ UnaryOp Not tmpName value
    return $ Variable tmpName
transpileExpr (Abs.EAlloc _ t expr) = do
    value <- transpileExpr expr
    tmpName <- getNewVariable $ TArray $ evalType t

    emit $ AllocArray tmpName value
    return $ Object tmpName
transpileExpr (Abs.EArrayElem _ expr1 expr2) = do
    array <- transpileExpr expr1
    value <- transpileExpr expr2

    t <- getTypeFromValue array >>= \case
        TArray t' -> return t'
        _ -> error "transpileExpr(Abs.EArrayElem): Array expected"

    tmpName <- getNewVariable t

    emit $ Load tmpName array value
    createValue tmpName
transpileExpr (Abs.EAttr _ expr1 (Abs.Ident attr)) = do
    object <- transpileExpr expr1

    case attr of
        "length" -> do
            tmpName <- getNewVariable TInt

            emit $ ArrayLength tmpName object
            return $ Variable tmpName
        _ -> do
            className <- getClassName object
            t <- asks $ Map.lookup className . globalClasses . cEnvGlobalTypes >>= \case
                Just classDef -> case Map.lookup attr $ classAttributes classDef of
                    Just t' -> return t'
                    _ -> error "transpileExpr(Abs.EAttr): Attribute not found"
                _ -> error "transpileExpr(Abs.EAttr): Class not found"
            tmpName <- getNewVariable t

            emit $ Get tmpName object className attr
            createValue tmpName
transpileExpr (Abs.ECastedNull _ t _) = do
    tmpName <- getNewVariable $ evalType t

    emit $ Assign tmpName (Constant 0)
    return $ Object tmpName
transpileExpr (Abs.ESelf _) = getSelf
transpileExpr (Abs.ECtor _ (Abs.Ident className)) = do
    tmpName <- getNewVariable $ TClass className

    emit $ AllocObject tmpName className
    return $ Object tmpName
transpileExpr (Abs.EMethod _ expr (Abs.Ident funcName) exprs) = do
    object <- transpileExpr expr
    values <- mapM transpileExpr exprs

    transpileMethodApp object funcName values
