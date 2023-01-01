{-# LANGUAGE LambdaCase #-}
module TypeChecker where

import Control.Monad ( foldM )
import Control.Monad.Except ( runExceptT, MonadError( throwError ), catchError )
import Control.Monad.Reader
    ( asks, MonadReader( local, ask ), ReaderT( runReaderT ) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor ( (<&>) )

import qualified Grammar.Abs as Abs

import TypeCheckerTypes ( GlobalTypes (..), TypeChecker, TEnv(..), TypeCheckError(..), TypeCheckException(..), Name, Type(..), ClassDef(..), FuncDef(..) )

mainFunctionName :: String
mainFunctionName = "main"

selfKeyword :: String
selfKeyword = "self"

arrayLengthAttribute :: String
arrayLengthAttribute = "length"


builtinFunctions :: Map.Map Name FuncDef
builtinFunctions = Map.fromList
    [ ("printInt", FuncDef TVoid [TInt])
    , ("printString", FuncDef TVoid [TString])
    , ("error", FuncDef TVoid [])
    , ("readInt", FuncDef TInt [])
    , ("readString", FuncDef TString [])
    ]

emptyTEnv :: TEnv
emptyTEnv = TEnv Map.empty Map.empty builtinFunctions Map.empty TVoid


rethrowError :: Abs.BNFC'Position -> TypeCheckException -> TypeCheckException
rethrowError position (TypeCheckException Nothing err) = TypeCheckException position err
rethrowError _ err = err


throwException :: TypeCheckError -> TypeChecker a
throwException = throwError . exception


exception :: TypeCheckError -> TypeCheckException
exception = TypeCheckException Nothing


findDuplicate :: [Name] -> Maybe Name
findDuplicate = \case
  [] -> Nothing
  (x:xs)
    | elem x xs -> Just x
    | otherwise -> findDuplicate xs


isTypeDeclared :: Name -> TEnv -> Bool
isTypeDeclared name env = Map.member name (tenvBlockVariables env) || Map.member name (tenvVariables env)

isTypeDeclaredInBlock :: Name -> TEnv -> Bool
isTypeDeclaredInBlock name env = Map.member name (tenvBlockVariables env)

getType :: Name -> TEnv -> Maybe Type
getType name env = case Map.lookup name (tenvBlockVariables env) of
  Just t -> Just t
  Nothing -> Map.lookup name (tenvVariables env)

setType :: Name -> Type -> TEnv -> TEnv
setType name t env = env { tenvVariables = Map.insert name t (tenvVariables env) }

setBlockType :: Name -> Type -> TEnv -> TEnv
setBlockType name t env = env { tenvBlockVariables = Map.insert name t (tenvBlockVariables env) }

isFuncDeclared :: Name -> TEnv -> Bool
isFuncDeclared name env = Map.member name (tenvFunctions env)

getFunc :: Name -> TEnv -> Maybe FuncDef
getFunc name env = Map.lookup name (tenvFunctions env)

setFunc :: Name -> FuncDef -> TEnv -> TEnv
setFunc name t env = env { tenvFunctions = Map.insert name t (tenvFunctions env) }

isClassDeclared :: Name -> TEnv -> Bool
isClassDeclared name env = Map.member name (tenvClasses env)

getClass :: Name -> TEnv -> Maybe ClassDef
getClass name env = Map.lookup name (tenvClasses env)

setClass :: Name -> ClassDef -> TEnv -> TEnv
setClass name t env = env { tenvClasses = Map.insert name t (tenvClasses env) }


assert :: Bool -> TypeCheckError -> TypeChecker ()
assert True _ = return ()
assert False err = throwError $ TypeCheckException Nothing err


assertWithEnv :: (TEnv -> Bool) -> TypeCheckError -> TypeChecker ()
assertWithEnv f err = asks f >>= \b -> assert b err


-- Execute type checking on syntax tree and rethrow any errors with replaced expression position.
errorHandlerDecorator :: Abs.HasPosition a => (a -> TypeChecker b) -> a -> TypeChecker b
errorHandlerDecorator func expr = catchError (func expr) $ throwError . rethrowError (Abs.hasPosition expr)


data BlockReturnType = BlockReturn Type
                     | BlockNoReturn


data StatementReturnType = StmtReturn Type
                         | StmtDeclaration (Map.Map Name Type)
                         | StmtNoReturn
                         | StmtInfiteLoop


evalBlock, evalBlock' :: Abs.Block -> TypeChecker BlockReturnType
evalBlock = errorHandlerDecorator evalBlock'

data BlockProcessingResult = BPRDeclarations (Map.Map Name Type)
                           | BPRReturn Type

evalBlock' (Abs.Block _ stmts) = foldM (\acc stmt ->
  case acc of
    BPRReturn _ -> return acc
    BPRDeclarations decls -> local (\env -> env {
        tenvBlockVariables = Map.union decls $ tenvBlockVariables env
      }) $ evalStatement stmt >>= \case
        StmtReturn t -> return $ BPRReturn t
        StmtDeclaration decls' -> case Map.toList $ Map.intersection decls decls' of
          [] -> return $ BPRDeclarations $ Map.union decls decls'
          (name, _):_ -> throwException $ TCVariableRedeclaration name
        StmtNoReturn -> return $ BPRDeclarations decls
        StmtInfiteLoop -> asks tenvReturnType <&> BPRReturn
  ) (BPRDeclarations Map.empty) stmts >>= \case
    BPRReturn t -> return $ BlockReturn t
    BPRDeclarations _ -> return BlockNoReturn


evalSingleStatement :: Abs.Stmt -> TypeChecker StatementReturnType
evalSingleStatement stmt = evalStatement stmt >>= \case
  StmtDeclaration _ -> throwException TCDeclarationInSingleStatement
  t -> return t

evalStatement, evalStatement' :: Abs.Stmt -> TypeChecker StatementReturnType
evalStatement = errorHandlerDecorator evalStatement'

evalStatement' (Abs.SExp _ expr) = evalExpression expr >> return StmtNoReturn
evalStatement' (Abs.Empty _) = return StmtNoReturn
evalStatement' (Abs.Ret _ expr) = do
  returnType <- asks tenvReturnType
  returnType' <- evalExpression expr
  isSameType returnType returnType'
  assert (returnType /= TVoid) TCReturnVoidType
  return $ StmtReturn returnType
evalStatement' (Abs.VRet _) = do
  returnType <- asks tenvReturnType
  isSameType returnType TVoid
  return $ StmtReturn TVoid
evalStatement' (Abs.Cond _ expr stmt) = evalExpression expr >>= \case
  TBool -> evalSingleStatement stmt >> return StmtNoReturn
  TLitBool True -> evalSingleStatement stmt
  TLitBool False -> evalSingleStatement stmt >> return StmtNoReturn
  t -> throwException $ TCTypeMismatch TBool t
evalStatement' (Abs.CondElse _ expr stmt1 stmt2) = evalExpression expr >>= \case
  TBool -> do
    ret1 <- evalSingleStatement stmt1
    ret2 <- evalSingleStatement stmt2
    case (ret1, ret2) of
      (StmtReturn t1, StmtReturn t2)
        |  t1 == t2 -> return $ StmtReturn t1
        | otherwise -> throwException $ TCTypeMismatch t1 t2
      _ -> return StmtNoReturn
  TLitBool True -> evalSingleStatement stmt2 >> evalSingleStatement stmt1
  TLitBool False -> evalSingleStatement stmt1 >> evalSingleStatement stmt2
  t -> throwException $ TCTypeMismatch TBool t
evalStatement' (Abs.While _ expr stmt) = evalExpression expr >>= \case
  TBool -> evalSingleStatement stmt
  TLitBool True -> evalSingleStatement stmt >>= \case
    StmtReturn t -> return $ StmtReturn t
    StmtDeclaration _ -> throwException TCDeclarationInSingleStatement
    _ -> return StmtInfiteLoop
  TLitBool False -> evalSingleStatement stmt >> return StmtNoReturn
  t -> throwException $ TCTypeMismatch TBool t
evalStatement' (Abs.BStmt _ block) = local (\env -> env {
      tenvBlockVariables = Map.empty,
      tenvVariables = Map.union (tenvBlockVariables env) (tenvVariables env)
  }) $ evalBlock block <&> \case
    BlockReturn t -> StmtReturn t
    BlockNoReturn -> StmtNoReturn
evalStatement' (Abs.ForLoop _ t (Abs.Ident varName) (Abs.Ident arrayName) stmt) = do
  arrayElemType <- asks (getType arrayName) >>= \case
    Just (TArray t') -> return t'
    Just t' -> throwException $ TCNotArray t'
    Nothing -> throwException $ TCVariableNotDeclared arrayName
  elemType <- evalType True t
  isSameType elemType arrayElemType
  local (setBlockType varName elemType) $ case stmt of
    (Abs.BStmt _ block) -> evalBlock block <&> \case
      BlockReturn ret -> StmtReturn ret
      BlockNoReturn -> StmtNoReturn
    _ -> evalSingleStatement stmt
evalStatement' (Abs.Ass _ lvalue expr) = do
  varType <- evalLValue lvalue
  exprType <- evalExpression expr
  isSameType varType exprType
  return StmtNoReturn
evalStatement' (Abs.Incr _ lvalue) = evalLValue lvalue >>= \case
  TInt -> return StmtNoReturn
  t -> throwException $ TCTypeMismatch TInt t
evalStatement' (Abs.Decr _ lvalue) = evalLValue lvalue >>= \case
  TInt -> return StmtNoReturn
  t -> throwException $ TCTypeMismatch TInt t
evalStatement' (Abs.Decl _ t items) = do
  itemType <- evalType True t
  foldM (\acc item -> do
      itemName <- local (\env -> env {
        tenvBlockVariables = Map.union acc $ tenvBlockVariables env
      }) $ evalItem itemType item
      assert (itemName /= selfKeyword) $ TCRestrictedKeyword selfKeyword
      assertWithEnv (not . isTypeDeclaredInBlock itemName) $ TCVariableRedeclaration itemName
      return $ Map.insert itemName itemType acc
    ) Map.empty items >>= \decls -> return $ StmtDeclaration decls

evalItem :: Type -> Abs.Item -> TypeChecker Name
evalItem _ (Abs.NoInit _ (Abs.Ident name)) = return name
evalItem itemType (Abs.Init _ (Abs.Ident name) expr) = do
  exprType <- evalExpression expr
  isSameType itemType exprType
  return name


evalLValue :: Abs.LValue -> TypeChecker Type
evalLValue (Abs.LValue _ (Abs.ESelf _)) = throwException TCNotLValue
evalLValue (Abs.LValue _ expr) = evalLValueExpr expr

evalLValueExpr', evalLValueExpr :: Abs.Expr -> TypeChecker Type
evalLValueExpr = errorHandlerDecorator evalLValueExpr'

evalLValueExpr' (Abs.EApp _ (Abs.Ident name) exprs) =
  asks (getFunc name) >>= \case
    Just t  -> evalFuncApp t exprs >>= \case
      t'@(TArray _) -> return t'
      t'@(TClass _) -> return t'
      _ -> throwException TCNotLValue
    Nothing -> throwException $ TCFunctionNotDeclared name
evalLValueExpr' (Abs.EMethod _ expr (Abs.Ident name) exprs) = do
  className <- evalLValueExpr expr >>= \case
    TClass t -> return t
    t -> throwException $ TCNotClass t
  methodDecl <- getMethod className name
  evalFuncApp methodDecl exprs >>= \case
    t'@(TArray _) -> return t'
    t'@(TClass _) -> return t'
    _ -> throwException TCNotLValue
evalLValueExpr' (Abs.ESelf _) = asks (getType selfKeyword) >>= \case
  Just t -> return t
  Nothing -> throwException TCSelfOutsideClass
evalLValueExpr' (Abs.EVar _ (Abs.Ident name)) = asks (getType name) >>= \case
  Just t -> return t
  Nothing -> throwException $ TCVariableNotDeclared name
evalLValueExpr' (Abs.EAttr _ expr (Abs.Ident name)) = evalLValueExpr expr >>= \case
  TArray _
    | name == arrayLengthAttribute -> throwException TCModifyingConstant
  TClass t -> getAttr t name
  t -> throwException $ TCNotClass t
evalLValueExpr' (Abs.EArrayElem _ expr1 expr2) = do
  expr1Type <- evalLValueExpr expr1
  expr2Type <- evalExpression expr2
  isSameType expr2Type TInt
  case expr1Type of
    TArray t -> return t
    t -> throwException $ TCNotArray t
evalLValueExpr' _ = throwException TCNotLValue



isSubclassOf :: Name -> Name -> TypeChecker Bool
isSubclassOf child parent = if child == parent
  then return True
  else asks (getClass child) >>= \case
    Just (ClassDef _ _ (Just parentName)) -> isSubclassOf parentName parent
    Just (ClassDef _ _ Nothing) -> return False
    Nothing -> throwException $ TCClassNotDeclared child

checkExpression :: Type -> Abs.Expr -> TypeChecker Type
checkExpression expectedType expr = do
  exprType <- evalExpression expr
  isSameType expectedType exprType
  return exprType

checkExpression_ :: Type -> Abs.Expr -> TypeChecker ()
checkExpression_ expectedType expr = checkExpression expectedType expr >> return ()

isSameType :: Type -> Type -> TypeChecker ()
isSameType expectedType exprType = do
  case (exprType, expectedType) of
    (TClass name, TClass parent) -> do
      isSubclass <- isSubclassOf name parent
      assert isSubclass $ TCTypeMismatch expectedType exprType
    _ -> assert (expectedType == exprType) $ TCTypeMismatch expectedType exprType


isSameTypeAnyWay :: Type -> Type -> TypeChecker ()
isSameTypeAnyWay t1 t2 = do
  case (t1, t2) of
    (TClass n1, TClass n2) -> do
      isSubclass1 <- isSubclassOf n1 n2
      isSubclass2 <- isSubclassOf n2 n1
      assert (isSubclass1 || isSubclass2) $ TCTypeMismatch t1 t2
    _ -> assert (t1 == t2) $ TCTypeMismatch t1 t2


evalFuncApp :: FuncDef -> [Abs.Expr] -> TypeChecker Type
evalFuncApp fnDecl exprs = do
  assert (length (funcArgs fnDecl) == length exprs) $ TCInvalidNumberOfArguments (length $ funcArgs fnDecl) (length exprs)
  mapM_ (\(expr, argType) -> do
    exprType <- evalExpression expr
    isSameType argType exprType
    ) $ zip exprs (funcArgs fnDecl)
  return $ funcReturnType fnDecl


getAttr :: Name -> Name -> TypeChecker Type
getAttr className attrName = asks (getClass className) >>= \case
  Just (ClassDef attrs _ _) -> case Map.lookup attrName attrs of
    Just t -> return t
    Nothing -> throwException $ TCNotClassAttribute className attrName
  Nothing -> throwException $ TCClassNotDeclared className

getMethod :: Name -> Name -> TypeChecker FuncDef
getMethod className methodName = asks (getClass className) >>= \case
  Just (ClassDef _ methods _) -> case Map.lookup methodName methods of
    Just fnDecl -> return fnDecl
    Nothing -> throwException $ TCNotClassMethod className methodName
  Nothing -> throwException $ TCClassNotDeclared className

evalExpression, evalExpression' :: Abs.Expr -> TypeChecker Type
evalExpression = errorHandlerDecorator evalExpression'

evalExpression' (Abs.EVar _ (Abs.Ident name)) = asks (getType name) >>= \case
  Just t -> return t
  Nothing -> throwException $ TCVariableNotDeclared name
evalExpression' (Abs.ELitInt _ i)
  | - (2 ^ (31 :: Integer)) <= i && i < 2 ^ (31 :: Integer)  = return $ TLitInt $ fromInteger i
  | otherwise       = throwException $ TCInvalidIntegerLiteral i
evalExpression' (Abs.ELitTrue _) = return $ TLitBool True
evalExpression' (Abs.ELitFalse _) = return $ TLitBool False
evalExpression' (Abs.ESelf _) = asks (getType selfKeyword) >>= \case
  Just t -> return t
  Nothing -> throwException TCSelfOutsideClass
evalExpression' (Abs.EApp _ (Abs.Ident name) exprs) =
  asks (getFunc name) >>= \case
    Just t  -> evalFuncApp t exprs
    Nothing -> throwException $ TCFunctionNotDeclared name
evalExpression' (Abs.EString _ s) = return $ TLitString s
evalExpression' (Abs.Neg _ expr) = evalExpression expr >>= \case
  TInt      -> return TInt
  TLitInt i -> return $ TLitInt (-i)
  t         -> throwException $ TCTypeMismatch TInt t
evalExpression' (Abs.Not _ expr) = evalExpression expr >>= \case
  TBool       -> return TBool
  TLitBool b  -> return $ TLitBool (not b)
  t           -> throwException $ TCTypeMismatch TBool t
evalExpression' (Abs.EMul _ expr1 op expr2) = do
  t1 <- evalExpression expr1
  t2 <- evalExpression expr2
  case (t1, t2) of
    (TLitInt i1, TLitInt i2) -> return $ TLitInt $ case op of
      Abs.Times _ -> i1 * i2
      Abs.Div _   -> i1 `div` i2
      Abs.Mod _   -> i1 `mod` i2
    _
      | t1 /= TInt  -> throwException $ TCTypeMismatch TInt t1
      | t2 /= TInt  -> throwException $ TCTypeMismatch TInt t2
      | otherwise   -> return TInt
evalExpression' (Abs.EAdd _ expr1 op expr2) = do
  t1 <- evalExpression expr1
  t2 <- evalExpression expr2
  case (t1, t2, op) of
    (TLitInt i1, TLitInt i2, _) -> return $ TLitInt $ case op of
      Abs.Plus _  -> i1 + i2
      Abs.Minus _ -> i1 - i2
    (TLitString s1, TLitString s2, Abs.Plus{}) -> return $ TLitString $ s1 ++ s2
    (_, _, Abs.Plus{})
      | t1 == t2 && t1 == TString -> return TString
      | t1 == t2 && t1 == TInt    -> return TInt
    _
      | t1 /= TInt  -> throwException $ TCTypeMismatch TInt t1
      | t2 /= TInt  -> throwException $ TCTypeMismatch TInt t2
      | otherwise   -> return TInt
evalExpression' (Abs.ERel _ expr1 op expr2) = do
  t1 <- evalExpression expr1
  t2 <- evalExpression expr2
  case (t1, t2) of
    (TVoid, _) -> throwException TCVoidComparison
    (TLitInt i1, TLitInt i2) -> return $ TLitBool $ case op of
      Abs.EQU _ -> i1 == i2
      Abs.NE _ -> i1 /= i2
      Abs.LTH _ -> i1 < i2
      Abs.LE _  -> i1 <= i2
      Abs.GTH _ -> i1 > i2
      Abs.GE _  -> i1 >= i2
    _ ->
      case op of
        Abs.EQU _ -> do
          isSameTypeAnyWay t1 t2
          return TBool
        Abs.NE _ -> do
          isSameTypeAnyWay t1 t2
          return TBool
        _
          | t1 /= TInt  -> throwException $ TCTypeMismatch TInt t1
          | t2 /= TInt  -> throwException $ TCTypeMismatch TInt t2
          | otherwise   -> return TBool
evalExpression' (Abs.EAnd _ expr1 expr2) = do
  t1 <- evalExpression expr1
  t2 <- evalExpression expr2
  case (t1, t2) of
    (TLitBool b1, TLitBool b2) -> return $ TLitBool $ b1 && b2
    _
      | t1 /= TBool -> throwException $ TCTypeMismatch TBool t1
      | t2 /= TBool -> throwException $ TCTypeMismatch TBool t2
      | otherwise -> return TBool
evalExpression' (Abs.EOr _ expr1 expr2) = do
  t1 <- evalExpression expr1
  t2 <- evalExpression expr2
  case (t1, t2) of
    (TLitBool b1, TLitBool b2) -> return $ TLitBool $ b1 || b2
    _
      | t1 /= TBool -> throwException $ TCTypeMismatch TBool t1
      | t2 /= TBool -> throwException $ TCTypeMismatch TBool t2
      | otherwise -> return TBool
evalExpression' (Abs.EArrayElem _ expr1 expr2) = do
  arrayType <- evalExpression expr1 >>= \case
    TArray t -> return t
    t -> throwException $ TCNotArray t
  exprType <- evalExpression expr2
  assert (exprType == TInt) $ TCTypeMismatch TInt exprType
  return arrayType
evalExpression' (Abs.EAttr _ expr (Abs.Ident name)) = evalExpression expr >>= \case
  TArray _
    | name == arrayLengthAttribute -> return TInt
  TClass t -> getAttr t name
  t -> throwException $ TCNotClass t
evalExpression' (Abs.EMethod _ expr (Abs.Ident name) exprs) = do
  className <- evalExpression expr >>= \case
    TClass t -> return t
    t -> throwException $ TCNotClass t
  methodDecl <- getMethod className name
  evalFuncApp methodDecl exprs
evalExpression' (Abs.ECtor _ (Abs.Ident name)) =
  asks (getClass name) >>= \case
    Just _ -> return $ TClass name
    Nothing -> throwException $ TCClassNotDeclared name
evalExpression' (Abs.EAlloc _ t expr) = do
  arrayType <- evalType True t
  checkExpression_ TInt expr >> return (TArray arrayType)
evalExpression' (Abs.ECastedNull _ t _) = do
  evalType True t >>= \case
    classType@(TClass className) -> do
      assertWithEnv (isClassDeclared className) $ TCClassNotDeclared className
      return classType
    TArray arrayType -> return $ TArray arrayType
    t' -> throwException $ TCNotCastable t'



checkFunction :: Maybe Type -> Abs.FnDef -> TypeChecker ()
checkFunction selfType = errorHandlerDecorator $ \(Abs.FnDef _ retType (Abs.Ident funcName) args body) -> do
  let initialEnvChange = case selfType of
        Just t  -> setBlockType selfKeyword t
        Nothing -> id

  envChanges <- foldM (\acc (Abs.Arg _ t (Abs.Ident n)) -> do
      t' <- evalType True t
      return $ setBlockType n t' . acc
      ) initialEnvChange args

  local envChanges $ do
    retType' <- evalType False retType
    returnType <- local (\env -> env { tenvReturnType = retType' }) $ evalBlock body >>= \case
      BlockReturn t -> return t
      BlockNoReturn -> do
        assert (retType' == TVoid) $ TCNotAllPathsReturn funcName
        return TVoid
    assert (retType' == returnType) $ TCReturnTypeError retType' returnType


type ClassesContextes = Map.Map Name (Map.Map Name Type, Map.Map Name FuncDef)

classContextToEnvChange :: ClassDef -> (TEnv -> TEnv)
classContextToEnvChange classDef env = env {
    tenvVariables = Map.union (classAttributes classDef) (tenvVariables env),
    tenvFunctions = Map.union (classMethods classDef) (tenvFunctions env)
  }


buildClassContext :: Name -> Set.Set Name -> ClassDef -> ClassesContextes -> TypeChecker ClassesContextes
buildClassContext className previousNames (ClassDef attrs methods parent) contextes = do
  (parentContextes, (vars, funcs)) <- case parent of
    Nothing -> return (contextes, (Map.empty, Map.empty))
    Just p
      | Set.member p previousNames -> throwException TCInheritanceCycle
      | otherwise -> case Map.lookup p contextes of
        Nothing -> do
          parentClassDef <- asks (getClass p) >>= \case
            Just t -> return t
            Nothing -> throwException $ TCClassNotDeclared p
          buildClassContext p (Set.insert p previousNames) parentClassDef contextes <&> \newContextes ->
            (newContextes, newContextes Map.! p)
        Just t -> return (contextes, t)
  mapM_ (\(methodName, method) -> case Map.lookup methodName funcs of
      Nothing -> return ()
      Just originalMethod -> assert (method == originalMethod) $ TCOverrideMethodSignatureMismatch methodName className
    ) $ Map.toList methods
  let redeclaredAttrs = Map.keysSet attrs `Set.intersection` Map.keysSet vars
  assert (Set.null redeclaredAttrs) $ TCAttributeRedeclaration (head $ Set.toList redeclaredAttrs) className
  return $ Map.insert className (Map.union attrs vars, Map.union methods funcs) parentContextes


classMethodsDefs :: Abs.ClassDef -> [Abs.FnDef]
classMethodsDefs classDef = map (\(Abs.ClassFnDef _ fnDef) -> fnDef) methods
  where
    methods = filter (\case
        Abs.ClassFnDef _ _ -> True
        _ -> False
      ) $ case classDef of
      Abs.ClassDefSimple _ _ m -> m
      Abs.ClassDefExtended _ _ _ m -> m


checkClass :: Name -> Abs.ClassDef -> ClassDef -> TypeChecker ()
checkClass className absClassDef classDef =
  local (classContextToEnvChange classDef) $ mapM_ (checkFunction (Just $ TClass className)) $ classMethodsDefs absClassDef


evalType :: Bool -> Abs.Type -> TypeChecker Type
evalType isVarType = errorHandlerDecorator $ \case
  Abs.Int _ -> return TInt
  Abs.Bool _ -> return TBool
  Abs.Str _ -> return TString
  Abs.Void _
    | not isVarType -> return TVoid
    | otherwise -> throwException TCVoidVariableType
  Abs.ClassType _ (Abs.Ident name) -> return $ TClass name
  -- Abs.Array _ (Abs.Array _ _) -> throwException TCMultiDimensionalArray
  Abs.Array _ t -> TArray <$> evalType isVarType t


evalAttrType :: Abs.AttrDef -> TypeChecker [(Name, Type)]
evalAttrType (Abs.AttrDef _ typeName names) = do
  t <- evalType True typeName
  return $ map (\(Abs.Ident n) -> (n, t)) names


evalFuncType, evalFuncType' :: Abs.FnDef -> TypeChecker FuncDef
evalFuncType = errorHandlerDecorator evalFuncType'
evalFuncType' (Abs.FnDef _ retType _ args _) = do
  let argNames = map (\(Abs.Arg _ _ (Abs.Ident n)) -> n) args
  case findDuplicate argNames of
    Just n -> throwException $ TCArgsNameDuplication n
    Nothing -> do
      let argTypes = map (\(Abs.Arg _ t _) -> t) args
      argTypes' <- mapM (evalType True) argTypes
      retType' <- evalType False retType
      return $ FuncDef retType' argTypes'


evalClassDefs :: [Abs.ClassTopDef] -> TypeChecker (Map.Map Name Type, Map.Map Name FuncDef)
evalClassDefs = foldM evalClassMemberDef (Map.empty, Map.empty)
  where
    evalClassMemberDef (types, funcs) = \case
      Abs.ClassAttrDef _ attr -> do
        attrs <- evalAttrType attr
        return (Map.union (Map.fromList attrs) types, funcs)
      Abs.ClassFnDef _ func@(Abs.FnDef _ _ (Abs.Ident name) _ _) -> do
        func' <- evalFuncType func
        assert (not $ Map.member name funcs) $ TCMethodAlreadyDeclared name
        return (types, Map.insert name func' funcs)


evalClassDef, evalClassDef' :: Abs.ClassDef -> TypeChecker ClassDef
evalClassDef = errorHandlerDecorator evalClassDef'

evalClassDef' (Abs.ClassDefSimple _ (Abs.Ident _) defs) = do
  (attrs, funcs) <- evalClassDefs defs
  return $ ClassDef attrs funcs Nothing

evalClassDef' (Abs.ClassDefExtended _ (Abs.Ident _) (Abs.Ident parent) defs) = do
  (attrs, funcs) <- evalClassDefs defs
  return $ ClassDef attrs funcs (Just parent)


classNameFromDef :: Abs.ClassDef -> Name
classNameFromDef (Abs.ClassDefSimple _ (Abs.Ident name) _) = name
classNameFromDef (Abs.ClassDefExtended _ (Abs.Ident name) _ _) = name

evalTypeChecker :: Abs.TopDef -> TypeChecker (TEnv -> TEnv)
evalTypeChecker = errorHandlerDecorator evalTypeChecker'

evalTypeChecker' :: Abs.TopDef -> TypeChecker (TEnv -> TEnv)
evalTypeChecker' (Abs.TopFnDef _ funcDef) = do
  assertWithEnv (not . isFuncDeclared name) $ TCFunctionAlreadyDeclared name
  evalFuncType funcDef <&> setFunc name
  where
    Abs.FnDef _ _ (Abs.Ident name) _ _ = funcDef

evalTypeChecker' (Abs.TopClassDef _ classDef) = do
  let name = classNameFromDef classDef
  assertWithEnv (not . isClassDeclared name) $ TCClassAlreadyDeclared name
  evalClassDef classDef <&> setClass name

mapGetter :: (Ord a) => Map.Map a b -> a -> b
mapGetter m k = case Map.lookup k m of
  Just v -> v
  Nothing -> error "mapGetter: key not found"

buildClassesAfterInheritance :: TEnv -> TypeChecker TEnv
buildClassesAfterInheritance env = do
  let classes = tenvClasses env
  newClassesContextes <- foldM (\contextes (name, classDef) -> case Map.lookup name contextes of
      Nothing -> buildClassContext name (Set.singleton name) classDef contextes
      _ -> return contextes
    ) Map.empty $ Map.toList classes

  return $ env { tenvClasses = Map.mapWithKey (\name classDef ->
      let (attrs, methods) = mapGetter newClassesContextes name in classDef {
        classAttributes = attrs,
        classMethods = methods
      }
    ) classes }


typeChecker :: Abs.Program -> TypeChecker (GlobalTypes, GlobalTypes)
typeChecker (Abs.Program _ topDefs) = do
  evalEnv <- foldM (\envTrans topDef ->
      local envTrans $ evalTypeChecker topDef <&> (.) envTrans
    ) id topDefs

  local evalEnv $ do
    env <- ask
    env' <- buildClassesAfterInheritance env

    e1 <- asks $ \e -> GlobalTypes {
      globalFunctions = tenvFunctions e,
      globalClasses = tenvClasses e
    }

    local (const env') $ do
      mainFunction <- asks $ getFunc mainFunctionName
      case mainFunction of
        Nothing -> throwException TCMainFunctionNotDeclared
        Just (FuncDef retType args) -> do
          assert (retType == TInt) TCMainFunctionReturnType
          assert (null args) TCMainFunctionArgs

      let functionsDefs = filter (\case Abs.TopFnDef{} -> True; _ -> False) topDefs
      mapM_ (\(Abs.TopFnDef _ fnDef) -> checkFunction Nothing fnDef) functionsDefs

      classes <- asks tenvClasses
      let classesDefs = filter (\case Abs.TopClassDef{} -> True; _ -> False) topDefs
      mapM_ (\(Abs.TopClassDef _ classDef) ->
          checkClass (classNameFromDef classDef) classDef (classes Map.! classNameFromDef classDef)
        ) classesDefs

      e2 <- asks $ \e -> GlobalTypes {
        globalFunctions = tenvFunctions e,
        globalClasses = tenvClasses e
      }

      return (e1, e2)


runTypeChecker :: TypeChecker a -> TEnv -> IO (Either TypeCheckException a)
runTypeChecker = runReaderT . runExceptT
