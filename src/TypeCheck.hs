module TypeCheck
  ( typeCheck
  , getType
  , TypeError(..)
  , TypeCheck.errorPos
  , typeOfNode
  ) where

import SymbolTable
import SyntaxTree as AST
import Type

import Data.Either.Combinators (leftToMaybe, mapRight, maybeToLeft, maybeToRight)
import Data.List (find, sortOn)
import Data.Map as M ((!), (!?), elems, lookup, member)

import Text.Parsec (SourcePos)

import Control.Applicative

type ActualType = Type

type ExpectedType = Type

type ExpectedAmount = Int

type ActualAmount = Int

type Name = String

data Context =
  Context SymbolTable ClassTable MethodTable

data TypeError
  = WrongTypeError ExpectedType ActualType SourcePos
  | UndefinedNameError Name SourcePos
  | WrongArgumentAmountError ExpectedAmount ActualAmount SourcePos
  | NotObjectTypeError ActualType SourcePos
  | MultiDimArrayError SourcePos
  | IntSizeError Integer SourcePos
  deriving (Show, Eq)

errorPos :: TypeError -> SourcePos
errorPos (WrongTypeError _ _ pos) = pos
errorPos (UndefinedNameError _ pos) = pos
errorPos (WrongArgumentAmountError _ _ pos) = pos
errorPos (NotObjectTypeError _ pos) = pos
errorPos (MultiDimArrayError pos) = pos
errorPos (IntSizeError _ pos) = pos

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
typeCheckVarDecl symTable (GenVarDecl typeNode _ _ pos) =
  assertTypeExists symTable (typeOfNode typeNode) pos

typeCheckStmt :: Context -> Stmt -> Either TypeError ()
typeCheckStmt c (ArrayAssign assignee index expr _) = do
  (Identifier' assignee) `hasType` IntArrayType $ c
  index `hasType` IntType $ c
  expr `hasType` IntType $ c
typeCheckStmt c (Assign assignee newVal pos) = do
  assigneeType <- getType c (Identifier' assignee)
  newValType <- getType c newVal
  assertTypeEq assigneeType newValType pos
typeCheckStmt c (Block stmts _) = mapM_ (typeCheckStmt c) stmts
typeCheckStmt c (If cond then' else' _) = do
  cond `hasType` BooleanType $ c
  typeCheckStmt c then'
  typeCheckStmt c else'
typeCheckStmt c (IfWithoutElse cond then' _) = do
  cond `hasType` BooleanType $ c
  typeCheckStmt c then'
-- TODO: type check expr
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
getType c (BinaryOp op l r _) = getTypeBinOp c op l r
getType c (Not expr _) = do
  expr `hasType` BooleanType $ c
  return BooleanType
getType c (ArrayLength array _) = do
  array `hasType` IntArrayType $ c
  return IntType
getType c (ArrayLookup array index _) = do
  array `hasType` IntArrayType $ c
  index `hasType` IntType $ c
  assertNotMultiDimArray array
  return IntType
  where
    assertNotMultiDimArray (NewArray _ pos) = Left $ MultiDimArrayError pos
    assertNotMultiDimArray _ = Right ()
getType _ (NewObject (Identifier typeName _) _) = Right $ ObjectType typeName
getType c (NewArray size _) = do
  size `hasType` IntType $ c
  return IntArrayType
getType c (Parens expr _) = getType c expr
getType _ (AST.False _) = Right BooleanType
getType _ (AST.True _) = Right BooleanType
getType _ (IntLit value pos) =
  if value > maxInt
    then Left $ IntSizeError value pos
    else Right IntType
  where
    maxInt = 2147483648
getType (Context symTable (ClassTable name _ _ _) _) (This _) =
  Right $ ObjectType . className . (!) symTable $ name
getType (Context _ classTable methodTable) (Identifier' (Identifier name pos)) =
  maybeToRight (UndefinedNameError name pos) . fmap varType $ getVar classTable methodTable name
getType c (MethodCall obj name args pos) = do
  actualArgTypes <- mapM (getType c) args
  methodTable <- getMethodTable obj name c
  let expectedArgTypes = map varType . sortOn varNo . elems . params $ methodTable
  assertTypeListEq expectedArgTypes actualArgTypes pos
  return $ returnType methodTable

getTypeBinOp :: Context -> Op -> Expr -> Expr -> Either TypeError Type
getTypeBinOp c op l r = checkOp c l r
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
  assertTypeEq ltype rtype (exprPos r)
  return returnType

getMethodTable :: Expr -> Identifier -> Context -> Either TypeError MethodTable
getMethodTable obj (Identifier methodName methodPos) c@(Context symTable _ _) = do
  type' <- getType c obj
  typeName <- objTypeName type'
  classTable <- maybeToRight (UndefinedNameError typeName (exprPos obj)) . (!?) symTable $ typeName
  maybeToRight (UndefinedNameError methodName methodPos) . M.lookup methodName . methods $
    classTable
  where
    objTypeName (ObjectType name) = Right $ name
    objTypeName t = Left $ NotObjectTypeError t (exprPos obj)

getVar :: ClassTable -> MethodTable -> String -> Maybe Var
getVar (ClassTable _ _ fields _) (MethodTable _ _ params locals _) name =
  locals !? name <|> params !? name <|> fields !? name

hasType :: Expr -> Type -> Context -> Either TypeError ()
hasType expr type' context = do
  exprType <- getType context expr
  assertTypeEq type' exprType (exprPos expr)

findDiff :: Eq t => [t] -> [t] -> Maybe (t, t)
findDiff x = find (\(a, b) -> a /= b) . zip x

-- TODO: Report better SourcePos in error
assertTypeListEq :: [Type] -> [Type] -> SourcePos -> Either TypeError ()
assertTypeListEq expected actual pos =
  if length expected /= length actual
    then Left $ WrongArgumentAmountError (length expected) (length actual) pos
    else maybeToLeft () . fmap (\(a, b) -> WrongTypeError a b pos) $ findDiff expected actual

assertTypeEq :: Type -> Type -> SourcePos -> Either TypeError ()
assertTypeEq expected actual pos =
  if expected /= actual
    then Left $ WrongTypeError expected actual pos
    else Right ()

assertTypeExists :: SymbolTable -> Type -> SourcePos -> Either TypeError ()
assertTypeExists symTable (ObjectType typeName) pos =
  if member typeName symTable
    then Right ()
    else Left $ UndefinedNameError typeName pos
assertTypeExists _ _ _ = Right ()
