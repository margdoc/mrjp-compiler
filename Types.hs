{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Control.Monad.Except ( ExceptT )
import Control.Monad.Reader ( ReaderT )
import qualified Data.Map as Map

import qualified Grammar.Abs as Abs

import Common ( showPosition )

type Name = String

data Type = TInt | TBool | TString| TArray Type | TClass Name | TVoid | TLitInt Int | TLitBool Bool | TLitString String

data FuncDef = FuncDef
  { funcReturnType :: Type
  , funcArgs :: [Type]
  } deriving (Eq)

data ClassDef = ClassDef
  { classAttributes :: Map.Map Name Type
  , classMethods    :: Map.Map Name FuncDef
  , parentClass     :: Maybe Name
  }

data TEnv = TEnv
  { tenvVariables      :: Map.Map Name Type
  , tenvBlockVariables :: Map.Map Name Type
  , tenvFunctions      :: Map.Map Name FuncDef
  , tenvClasses        :: Map.Map Name ClassDef
  , tenvReturnType     :: Type
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
                    | TCMethodAlreadyDeclared Name
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
                    | TCAddOperatorTypeMismatch Type Type String
                    | TCFunctionType
                    | TCInvalidNumberOfArguments Int Int
                    | TCDeclarationInSingleStatement
                    | TCVoidVariableType
                    | TCReturnVoidType
                    | TCInvalidIntegerLiteral Integer
                    | TCInheritanceCycle
                    | TCNotLValue
                    | TCModifyingConstant
                    | TCNotCastable Type
                    | TCOverrideMethodSignatureMismatch Name
                    | TCSelfOutsideClass
                    | TCRestrictedKeyword Name

instance Eq Type where
  (==) TVoid TVoid = True
  (==) TInt TInt = True
  (==) (TLitInt _) (TLitInt _) = True
  (==) TInt (TLitInt _) = True
  (==) (TLitInt _) TInt = True
  (==) TBool TBool = True
  (==) (TLitBool _) (TLitBool _) = True
  (==) TBool (TLitBool _) = True
  (==) (TLitBool _) TBool = True
  (==) TString TString = True
  (==) (TLitString _) (TLitString _) = True
  (==) TString (TLitString _) = True
  (==) (TLitString _) TString = True
  (==) (TArray t1) (TArray t2) = t1 == t2
  (==) (TClass c1) (TClass c2) = c1 == c2
  (==) _ _ = False


instance Show Type where
  showsPrec _ TVoid = showString "void"
  showsPrec _ TInt = showString "int"
  showsPrec _ (TLitInt i) = showString "int (" . shows i . showString ")"
  showsPrec _ TBool = showString "boolean"
  showsPrec _ (TLitBool b) = showString "boolean (" . shows b . showString ")"
  showsPrec _ TString = showString "string"
  showsPrec _ (TLitString s) = showString "string (" . shows s . showString ")"
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
  show (TCMethodAlreadyDeclared name) = "Method " ++ name ++ " already declared"
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
  show (TCAddOperatorTypeMismatch t1 t2 op) = "Type " ++ show t1 ++ " does not match expected type " ++ show t2 ++ " for " ++ op ++ " operator"
  show TCFunctionType = "Function type is not supported"
  show (TCInvalidNumberOfArguments expected actual) = "Invalid number of arguments: expected " ++ show expected ++ ", actual " ++ show actual
  show TCDeclarationInSingleStatement = "Variable declaration in single statement is not supported"
  show TCVoidVariableType = "Variable cannot be of type void"
  show TCReturnVoidType = "Returned type cannot be void"
  show (TCInvalidIntegerLiteral i) = "Invalid integer literal: " ++ show i
  show TCInheritanceCycle = "Inheritance cycle"
  show TCNotLValue = "Expression is not an lvalue"
  show TCModifyingConstant = "Cannot modify constant"
  show (TCNotCastable t) = "Type " ++ show t ++ " is not castable"
  show (TCOverrideMethodSignatureMismatch name) = "Method " ++ name ++ " has a different signature in a parent class"
  show TCSelfOutsideClass = "self is only allowed outside a class context"
  show (TCRestrictedKeyword name) = "Restricted keyword " ++ name ++ " cannot be used as an identifier"
