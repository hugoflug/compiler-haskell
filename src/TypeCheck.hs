module TypeCheck
  ( typeCheck
  , getType
  , TypeError(..)
  ) where

import SymbolTable
import SyntaxTree as AST
import Type

import Data.Either.Combinators (leftToMaybe, mapRight, maybeToLeft, maybeToRight)
import Data.Foldable (asum)
import Data.List (find)
import Data.Map as M ((!), (!?), elems, lookup, member)
import Data.Maybe (fromJust)

import Control.Applicative

type ActualType = Type

type ExpectedType = Type

type ExpectedAmount = Int

type ActualAmount = Int

type Name = String

data Context =
  Context SymbolTable ClassTable MethodTable

data TypeError
  = WrongTypeError ExpectedType ActualType
  | UndefinedNameError Name
  | WrongArgumentAmountError ExpectedAmount ActualAmount
  deriving (Show, Eq)

typeCheck :: SymbolTable -> Program -> Maybe TypeError
typeCheck symTable (Program mainClass classDecls _) = leftToMaybe result
  where
    result = do
      typeCheckMainClass symTable mainClass
      mapM_ (typeCheckClassDecl symTable) classDecls

typeCheckMainClass :: SymbolTable -> MainClass -> Either TypeError ()
typeCheckMainClass symTable (MainClass (Identifier name _) _ varDecls stmts _) = do
  mapM_ (typeCheckVarDecl symTable) $ varDecls
  mapM_ (typeCheckStmt context) $ stmts
  where
    context = Context symTable (classTable) (methods classTable ! "main")
    classTable = symTable ! name

typeCheckClassDecl :: SymbolTable -> ClassDecl -> Either TypeError ()
typeCheckClassDecl symTable (ClassDecl (Identifier name _) varDecls methodDecls _) = do
  mapM_ (typeCheckVarDecl symTable) varDecls
  mapM_ (typeCheckMethodDecl symTable (symTable ! name)) methodDecls

typeCheckMethodDecl :: SymbolTable -> ClassTable -> MethodDecl -> Either TypeError ()
typeCheckMethodDecl symTable class' (MethodDecl returnType (Identifier name _) formals varDecls stmts returnVal _) = do
  mapM_ (typeCheckVarDecl symTable) varDecls
  mapM_ (typeCheckStmt context) stmts
  mapM_ (typeCheckVarDecl symTable) formals
  returnVal `hasType` (typeOfNode returnType) $ context
  where
    context = Context symTable class' (methods class' ! name)

typeCheckVarDecl :: SymbolTable -> GenVarDecl -> Either TypeError ()
typeCheckVarDecl symTable (GenVarDecl typeNode _ _ _) =
  assertTypeExists symTable (typeOfNode typeNode)

typeCheckStmt :: Context -> Stmt -> Either TypeError ()
typeCheckStmt c (ArrayAssign assignee index expr _) = do
  (Identifier' assignee) `hasType` IntArrayType $ c
  index `hasType` IntType $ c
  expr `hasType` IntType $ c
typeCheckStmt c (Assign assignee newVal _) = do
  assigneeType <- getType c (Identifier' assignee)
  newValType <- getType c newVal
  assertTypeEq assigneeType newValType
typeCheckStmt c (Block stmts _) = mapM_ (typeCheckStmt c) stmts
typeCheckStmt c (If cond then' else' _) = do
  cond `hasType` BooleanType $ c
  typeCheckStmt c then'
  typeCheckStmt c else'
typeCheckStmt c (IfWithoutElse cond then' _) = do
  cond `hasType` BooleanType $ c
  typeCheckStmt c then'
typeCheckStmt c (Syso expr _) = mapRight (const ()) $ typeCheckExpr c expr
typeCheckStmt c (While cond stmt _) = do
  cond `hasType` BooleanType $ c
  typeCheckStmt c stmt

typeOfNode :: TypeNode -> Type
typeOfNode (BooleanTypeNode _) = BooleanType
typeOfNode (IntTypeNode _) = IntType
typeOfNode (IntArrayTypeNode _) = IntArrayType
typeOfNode (ObjectTypeNode typeName _) = ObjectType typeName

typeCheckExpr :: Context -> Expr -> Either TypeError ()
typeCheckExpr c e = mapRight (const ()) $ getType c e

getType :: Context -> Expr -> Either TypeError Type
getType c (BinaryOp' binOp) = typeCheckBinOp c binOp
getType c (Not expr _) = getType c expr
getType c (ArrayLength array _) = do
  array `hasType` IntArrayType $ c
  return IntType
getType c (ArrayLookup array index _) = do
  array `hasType` IntArrayType $ c
  index `hasType` IntType $ c
  return IntType
getType c (NewObject (Identifier typeName _) _) = Right $ ObjectType typeName
getType c (NewArray size _) = do
  size `hasType` IntType $ c
  return IntArrayType
getType c (Parens expr _) = getType c expr
getType c (AST.False _) = Right BooleanType
getType c (AST.True _) = Right BooleanType
getType c (IntLit _ _) = Right IntType
getType (Context _ classTable methodTable) (Identifier' (Identifier name _)) =
  maybeToRight (UndefinedNameError name) . fmap varType $ getVar classTable methodTable name
getType c (MethodCall obj name args _) = do
  actualArgTypes <- mapM (getType c) args
  methodTable <- getMethodTable obj name c
  let expectedArgTypes = map varType . elems . params $ methodTable
  assertTypeListEq expectedArgTypes actualArgTypes
  return $ returnType methodTable

typeCheckBinOp :: Context -> BinaryOp -> Either TypeError Type
typeCheckBinOp c (BinaryOp op l r _) = checkOp c l r
  where
    checkOp =
      case op of
        Mult -> getTypeOp IntType IntType
        Plus -> getTypeOp IntType IntType
        Minus -> getTypeOp IntType IntType
        LessThan -> getTypeOp IntType BooleanType
        LessOrEqualThan -> getTypeOp IntType BooleanType
        GreaterOrEqualThan -> getTypeOp IntType BooleanType
        GreaterThan -> getTypeOp IntType BooleanType
        Equal -> getTypeGenericOp BooleanType
        NotEqual -> getTypeGenericOp BooleanType
        And -> getTypeOp BooleanType BooleanType
        Or -> getTypeOp BooleanType BooleanType

getTypeOp :: Type -> Type -> Context -> Expr -> Expr -> Either TypeError Type
getTypeOp opType returnType c l r = do
  l `hasType` opType $ c
  r `hasType` opType $ c
  return $ returnType

getTypeGenericOp :: Type -> Context -> Expr -> Expr -> Either TypeError Type
getTypeGenericOp returnType c l r = do
  ltype <- getType c l
  rtype <- getType c r
  assertTypeEq ltype rtype
  return returnType

getMethodTable :: Expr -> Method -> Context -> Either TypeError MethodTable
getMethodTable obj methodName c@(Context symTable _ _) = do
  type' <- getType c obj
  typeName <- objTypeName type'
  classTable <- maybeToRight (UndefinedNameError typeName) . (!?) symTable $ typeName
  maybeToRight (UndefinedNameError methodName) . M.lookup methodName . methods $ classTable
  where
    objTypeName (ObjectType name) = Right $ name
    objTypeName t = Left $ WrongTypeError (ObjectType "") t

getVar :: ClassTable -> MethodTable -> String -> Maybe Var
getVar (ClassTable _ _ fields) (MethodTable _ _ params locals) name =
  fields !? name <|> params !? name <|> locals !? name

hasType :: Expr -> Type -> Context -> Either TypeError ()
hasType expr type' context = do
  exprType <- getType context expr
  assertTypeEq type' exprType

findDiff :: Eq t => [t] -> [t] -> Maybe (t, t)
findDiff x = find (\(a, b) -> a /= b) . zip x

assertTypeListEq :: [Type] -> [Type] -> Either TypeError ()
assertTypeListEq expected actual =
  if length expected /= length actual
    then Left $ WrongArgumentAmountError (length expected) (length actual)
    else maybeToLeft () . fmap (\(a, b) -> WrongTypeError a b) $ findDiff expected actual

assertTypeEq :: Type -> Type -> Either TypeError ()
assertTypeEq expected actual =
  if expected /= actual
    then Left $ WrongTypeError expected actual
    else Right ()

assertTypeExists :: SymbolTable -> Type -> Either TypeError ()
assertTypeExists symTable (ObjectType typeName) =
  if member typeName symTable
    then Right ()
    else Left $ UndefinedNameError typeName
assertTypeExists _ type' = Right ()
