module TypeCheck
  ( typeCheck
  , getType
  , TypeError()
  ) where

import SymbolTable
import SyntaxTree as AST
import Type

import Data.Foldable (asum)
import Data.Function ((&))
import Data.List (find)
import Data.Map ((!), (!?), elems, member)
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
typeCheck symTable (Program mainClass classDecls _) =
  typeCheckMainClass symTable mainClass <|> (asum . fmap (typeCheckClassDecl symTable)) classDecls

typeCheckMainClass :: SymbolTable -> MainClass -> Maybe TypeError
typeCheckMainClass symTable (MainClass (Identifier name _) _ varDecls stmts _) =
  (asum . fmap (typeCheckVarDecl symTable)) varDecls <|> (asum . fmap (typeCheckStmt context)) stmts
  where
    classTable = symTable ! name
    context = Context symTable classTable (methods classTable ! "main")
    methods (ClassTable _ m _) = m

typeCheckClassDecl :: SymbolTable -> ClassDecl -> Maybe TypeError
typeCheckClassDecl symTable (ClassDecl (Identifier name _) varDecls methodDecls _) =
  (asum . fmap (typeCheckVarDecl symTable)) varDecls <|>
  (asum . fmap (typeCheckMethodDecl symTable (symTable ! name))) methodDecls

typeCheckMethodDecl :: SymbolTable -> ClassTable -> MethodDecl -> Maybe TypeError
typeCheckMethodDecl symTable class' (MethodDecl returnType (Identifier name _) formals varDecls stmts returnVal _) =
  (asum . fmap (typeCheckVarDecl symTable)) varDecls <|> (asum . fmap (typeCheckStmt context)) stmts <|>
  (asum . fmap (typeCheckVarDecl symTable)) formals <|>
  (returnVal `hasType` (typeOfNode returnType)) context
  where
    context = Context symTable class' (methods class' ! name)
    methods (ClassTable _ m _) = m

typeCheckVarDecl :: SymbolTable -> GenVarDecl -> Maybe TypeError
typeCheckVarDecl symTable (GenVarDecl typeNode _ _ _) =
  assertTypeExists symTable (typeOfNode typeNode)

typeCheckStmt :: Context -> Stmt -> Maybe TypeError
typeCheckStmt c (ArrayAssign assignee index expr _) =
  (Identifier' assignee `hasType` IntArrayType) c <|> (index `hasType` IntType) c <|>
  (expr `hasType` IntType) c
typeCheckStmt c (Assign assignee newVal _) =
  assertTypeEq (getType c (Identifier' assignee)) (getType c newVal)
typeCheckStmt c (Block stmts _) = (asum . fmap (typeCheckStmt c)) stmts
typeCheckStmt c (If cond then' else' _) =
  (cond `hasType` BooleanType) c <|> typeCheckStmt c then' <|> typeCheckStmt c else'
typeCheckStmt c (IfWithoutElse cond then' _) =
  (cond `hasType` BooleanType) c <|> typeCheckStmt c then'
typeCheckStmt c (Syso expr _) = typeCheckExpr c expr
typeCheckStmt c (While cond stmt _) = (cond `hasType` BooleanType) c <|> typeCheckStmt c stmt

typeOfNode :: TypeNode -> Type
typeOfNode (BooleanTypeNode _) = BooleanType
typeOfNode (IntTypeNode _) = IntType
typeOfNode (IntArrayTypeNode _) = IntArrayType
typeOfNode (ObjectTypeNode typeName _) = ObjectType typeName

typeCheckExpr :: Context -> Expr -> Maybe TypeError
typeCheckExpr c (BinaryOp' binOp) = typeCheckBinOp c binOp
typeCheckExpr c (Not expr _) = typeCheckExpr c expr
typeCheckExpr c (ArrayLength array _) = (array `hasType` IntArrayType) c <|> typeCheckExpr c array
typeCheckExpr c (ArrayLookup array index _) =
  (array `hasType` IntArrayType) c <|> (index `hasType` IntType) c <|> typeCheckExpr c array
typeCheckExpr c (NewObject _ _) = Nothing
typeCheckExpr c (NewArray size _) = (size `hasType` IntType) c <|> typeCheckExpr c size
typeCheckExpr c (Parens expr _) = typeCheckExpr c expr
typeCheckExpr c (AST.False _) = Nothing
typeCheckExpr c (AST.True _) = Nothing
typeCheckExpr c (IntLit _ _) = Nothing
typeCheckExpr (Context _ classTable methodTable) (Identifier' (Identifier name _)) =
  flipMaybe (UndefinedNameError name) $ getVar classTable methodTable name
typeCheckExpr c@(Context symTable _ _) (MethodCall obj name args _) =
  assertTypeListEq expected actual
  where
    actual = map (getType c) args
    expected = map varType . elems . params $ getMethodTable obj name c
    params (MethodTable _ _ p _) = p
    varType (Var _ type' _) = type'

typeCheckBinOp :: Context -> BinaryOp -> Maybe TypeError
typeCheckBinOp c (BinaryOp op l r _) = checkOp c l r
  where
    checkOp =
      case op of
        Mult -> intOp
        Plus -> intOp
        Minus -> intOp
        LessThan -> intOp
        LessOrEqualThan -> intOp
        GreaterOrEqualThan -> intOp
        GreaterThan -> intOp
        Equal -> genericOp
        NotEqual -> genericOp
        And -> boolOp
        Or -> boolOp

intOp :: Context -> Expr -> Expr -> Maybe TypeError
intOp c l r = (l `hasType` IntType) c <|> (r `hasType` IntType) c

boolOp :: Context -> Expr -> Expr -> Maybe TypeError
boolOp c l r = (l `hasType` BooleanType) c <|> (r `hasType` BooleanType) c

genericOp :: Context -> Expr -> Expr -> Maybe TypeError
genericOp c l r = assertTypeEq (getType c l) (getType c r)

getType :: Context -> Expr -> Type
getType _ (Not _ _) = BooleanType
getType _ (ArrayLength _ _) = IntType
getType _ (ArrayLookup _ _ _) = IntType
getType _ (NewObject (Identifier typeName _) _) = ObjectType typeName
getType _ (NewArray _ _) = IntArrayType
getType c (Parens expr _) = getType c expr
getType _ (AST.False _) = BooleanType
getType _ (AST.True _) = BooleanType
getType _ (IntLit _ _) = IntType
getType c (MethodCall obj name _ _) = methodType $ getMethodTable obj name c
  where
    methodType (MethodTable _ t _ _) = t
getType (Context _ classTable methodTable) (Identifier' (Identifier name _)) =
  varType . fromJust $ getVar classTable methodTable name
  where
    varType (Var _ type' _) = type'

getMethodTable :: Expr -> Method -> Context -> MethodTable
getMethodTable obj name c@(Context symTable _ _) =
  flip (!) name . methods . (!) symTable . objTypeName . getType c $ obj
  where
    methods (ClassTable _ m _) = m
    objTypeName (ObjectType name) = name
    objTypeName _ = error "Not an object type"

getVar :: ClassTable -> MethodTable -> String -> Maybe Var
getVar (ClassTable _ _ fields) (MethodTable _ _ params locals) name =
  fields !? name <|> params !? name <|> locals !? name

getTypeBinOp :: Context -> BinaryOp -> Type
getTypeBinOp c (BinaryOp op l r _) =
  case op of
    Mult -> IntType
    Plus -> IntType
    Minus -> IntType
    LessThan -> BooleanType
    LessOrEqualThan -> BooleanType
    GreaterOrEqualThan -> BooleanType
    GreaterThan -> BooleanType
    Equal -> BooleanType
    NotEqual -> BooleanType
    And -> BooleanType
    Or -> BooleanType

hasType :: Expr -> Type -> Context -> Maybe TypeError
hasType expr type' context = assertTypeEq type' $ getType context expr

assertTypeListEq :: [Type] -> [Type] -> Maybe TypeError
assertTypeListEq expected actual =
  if length expected /= length actual
    then Just $ WrongArgumentAmountError (length expected) (length actual)
    else fmap (\(a, b) -> WrongTypeError a b) . find (\(a, b) -> a /= b) $ zip expected actual

assertTypeEq :: Type -> Type -> Maybe TypeError
assertTypeEq expected actual =
  if expected /= actual
    then Just $ WrongTypeError expected actual
    else Nothing

assertTypeExists :: SymbolTable -> Type -> Maybe TypeError
assertTypeExists symTable (ObjectType typeName) =
  if member typeName symTable
    then Nothing
    else Just $ UndefinedNameError typeName
assertTypeExists _ _ = Nothing

flipMaybe :: a -> Maybe b -> Maybe a
flipMaybe _ (Just _) = Nothing
flipMaybe x Nothing = Just x
