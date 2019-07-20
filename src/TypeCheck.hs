module TypeCheck
  ( typeCheck
  , getType
  ) where

import Errors
import SymbolTable
import SyntaxTree as AST
import Type

import Data.Map ((!), (!?))
import Data.Maybe (fromJust)

import Control.Applicative

data Context =
  Context SymbolTable (Maybe ClassTable) (Maybe MethodTable)

typeCheck :: SymbolTable -> Program -> Maybe TypeError
typeCheck symTable program = typeCheck' (Context symTable Nothing Nothing) (Program' program)

typeCheck' :: Context -> SyntaxTreeNode -> Maybe TypeError
typeCheck' c (Expr expr) = typeCheckExpr c expr

typeCheckExpr :: Context -> Expr -> Maybe TypeError
typeCheckExpr c (BinaryOp' binOp) = typeCheckBinOp c binOp
typeCheckExpr c (Not expr _) = typeCheckExpr c expr
typeCheckExpr c (ArrayLength array _) = (array `shouldEq` IntArrayType) c <|> typeCheckExpr c array
typeCheckExpr c (ArrayLookup array index _) =
  (array `shouldEq` IntArrayType) c <|> (index `shouldEq` IntType) c <|> typeCheckExpr c array
typeCheckExpr c (NewObject _ _) = Nothing
typeCheckExpr c (NewArray size _) = (size `shouldEq` IntType) c <|> typeCheckExpr c size
typeCheckExpr c (Parens expr _) = typeCheckExpr c expr
typeCheckExpr c (AST.False _) = Nothing
typeCheckExpr c (AST.True _) = Nothing
typeCheckExpr c (IntLit _ _) = Nothing
typeCheckExpr (Context _ classTable methodTable) (Identifier' (Identifier name _)) =
  flipMaybe (UndefinedNameError name) $ getVar classTable methodTable name

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
intOp c l r = (l `shouldEq` IntType) c <|> (r `shouldEq` IntType) c

boolOp :: Context -> Expr -> Expr -> Maybe TypeError
boolOp c l r = (l `shouldEq` BooleanType) c <|> (r `shouldEq` BooleanType) c

genericOp :: Context -> Expr -> Expr -> Maybe TypeError
genericOp c l r = assertTypeEq (getType' c l) (getType' c r)

getType :: SymbolTable -> Expr -> Type
getType = undefined

getType' :: Context -> Expr -> Type
getType' _ (Not _ _) = BooleanType
getType' _ (ArrayLength _ _) = IntType
getType' _ (ArrayLookup _ _ _) = IntType
getType' _ (NewObject (Identifier typeName _) _) = ObjectType typeName
getType' _ (NewArray _ _) = IntArrayType
getType' c (Parens expr _) = getType' c expr
getType' _ (AST.False _) = BooleanType
getType' _ (AST.True _) = BooleanType
getType' _ (IntLit _ _) = IntType
getType' c@(Context symTable _ _) (MethodCall obj name args _) =
  methodType . flip (!) name . methods . (!) symTable . objTypeName . getType' c $ obj
  where
    methodType (MethodTable _ t _ _) = t
    methods (ClassTable _ m _) = m
    objTypeName (ObjectType name) = name
getType' (Context _ classTable methodTable) (Identifier' (Identifier name _)) =
  varType . fromJust $ getVar classTable methodTable name
  where
    varType (Var _ type' _) = type'

getVar :: Maybe ClassTable -> Maybe MethodTable -> String -> Maybe Var
getVar (Just (ClassTable _ _ fields)) (Just (MethodTable _ _ params locals)) name =
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

shouldEq :: Expr -> Type -> Context -> Maybe TypeError
shouldEq expr type' context = assertTypeEq type' $ getType' context expr

--assertType :: Context -> Type -> Expr -> Maybe TypeError
--assertType context expectedType = assertTypeEq expectedType . getType' context
assertTypeEq :: Type -> Type -> Maybe TypeError
assertTypeEq expected actual =
  if expected /= actual
    then Just $ WrongTypeError expected actual
    else Nothing

flipMaybe _ (Just _) = Nothing
flipMaybe x Nothing = Just x
