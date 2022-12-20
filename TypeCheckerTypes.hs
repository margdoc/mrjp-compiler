module TypeCheckerTypes where

import BasePrelude (intercalate)
import Control.Monad.State ( StateT )
import Control.Monad.Except ( ExceptT )
import Control.Monad.Reader ( ReaderT )
import qualified Data.Map as Map
import Data.List ( foldl' )

import qualified Grammar.Abs as Abs

import Common ( showPosition )

type Name = String

data Type = TInt | TBool | TString | TArray Type | TClass Name | TVoid
  deriving (Eq)

data FuncDef = FuncDef
  { funcReturnType :: Type
  , funcArgs :: [Type]
  }

data ClassDef = ClassDef
  { classAttributes :: Map.Map Name Type
  , classMethods    :: Map.Map Name FuncDef
  , parentClass     :: Maybe Name
  }

data TEnv = TEnv
  { tenvVariables :: Map.Map Name Type
  , tenvFunctions :: Map.Map Name FuncDef
  , tenvClasses   :: Map.Map Name ClassDef
  , tenvReturnType :: Type
  }

type TypeChecker = ExceptT TypeCheckException (ReaderT TEnv IO)

data TypeCheckException = TypeCheckException Abs.BNFC'Position TypeCheckError
data TypeCheckError = TCVariableNotDeclared Name
                    | TCFunctionNotDeclared Name
                    | TCClassNotDeclared Name
                    | TCArgsNameDuplication Name
                    | TCMultiDimensionalArray
                    | TCClassAlreadyDeclared Name
                    | TCClassAttributeAlreadyDeclared Name
                    | TCClassMethodAlreadyDeclared Name
                    | TCFunctionAlreadyDeclared Name
                    | TCMainFunctionNotDeclared
                    | TCMainFunctionReturnType
                    | TCMainFunctionArgs
                    | TCReturnTypeError Type Type
                    | TCNotAllPathsReturn
                    | TCTypeMismatch Type Type
                    | TCNotArray Type
                    | TCVariableRedeclaration Name
                    | TCNotClass Type
                    | TCNotClassAttribute Name Name
                    | TCNotClassMethod Name Name


instance Show Type where
  showsPrec _ TVoid = showString "void"
  showsPrec _ TInt = showString "int"
  showsPrec _ TBool = showString "boolean"
  showsPrec _ TString = showString "string"
  showsPrec _ (TArray t) = shows t . showString "[]"
  showsPrec _ (TClass name) = showString name


instance Show TypeCheckException where
  show (TypeCheckException position err) = show err ++ showPosition position

instance Show TypeCheckError where
  show (TCVariableNotDeclared name) = "Variable " ++ name ++ " not declared"
  show (TCFunctionNotDeclared name) = "Function " ++ name ++ " not declared"
  show (TCClassNotDeclared name) = "Class " ++ name ++ " not declared"
  show (TCArgsNameDuplication name) = "Argument " ++ name ++ " is duplicated"
  show TCMultiDimensionalArray = "Multi-dimensional arrays are not supported"
  show (TCClassAlreadyDeclared name) = "Class " ++ name ++ " already declared"
  show (TCClassAttributeAlreadyDeclared name) = "Class attribute " ++ name ++ " already declared"
  show (TCClassMethodAlreadyDeclared name) = "Class method " ++ name ++ " already declared"
  show (TCFunctionAlreadyDeclared name) = "Function " ++ name ++ " already declared"
  show TCMainFunctionNotDeclared = "Main function not declared"
  show TCMainFunctionReturnType = "Main function must return int"
  show TCMainFunctionArgs = "Main function must have no arguments"
  show (TCReturnTypeError expected actual) = "Return type " ++ show actual ++ " does not match expected type " ++ show expected
  show TCNotAllPathsReturn = "Not all paths return a value"
  show (TCTypeMismatch expected actual) = "Type " ++ show actual ++ " does not match expected type " ++ show expected
  show (TCNotArray t) = "Type " ++ show t ++ " is not an array"
  show (TCVariableRedeclaration name) = "Variable " ++ name ++ " is already declared"
  show (TCNotClass t) = "Type " ++ show t ++ " is not a class"
  show (TCNotClassAttribute t name) = "Class " ++ t ++ " does not have attribute " ++ name
  show (TCNotClassMethod t name) = "Class " ++ t ++ " does not have method " ++ name

instance Show FuncDef where
  show (FuncDef returnType args) = show returnType ++ "(" ++ intercalate ", " (map show args) ++ ")"

instance Show ClassDef where
  show (ClassDef attributes methods parentClass) = "ClassDef { attributes = " ++ show attributes ++ ", methods = " ++ show methods ++ ", parentClass = " ++ show parentClass ++ " }"

instance Show TEnv where
  show (TEnv variables functions classes t) = "TEnv { variables = " ++ show variables ++ ", functions = " ++ show functions ++ ", classes = " ++ show classes ++ ", t = " ++ show t ++ " }"
