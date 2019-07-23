module SyntaxTree where

import Text.Parsec (SourcePos)

type ArgDefs = [GenVarDecl]

type ReturnType = TypeNode

type ReturnVal = Expr

type MethodName = Identifier

type Method = String

type ArgList = [Expr]

type Size = Expr

type Assignee = Identifier

type Index = Expr

type ClassName = Identifier

type StdArgName = String

type VarName = Identifier

type TypeName = Identifier

data MethodDecl =
  MethodDecl ReturnType MethodName ArgDefs [GenVarDecl] [Stmt] ReturnVal SourcePos
  deriving (Show, Eq)

data ClassDecl =
  ClassDecl ClassName [GenVarDecl] [MethodDecl] SourcePos
  deriving (Show, Eq)

data MainClass =
  MainClass ClassName StdArgName [GenVarDecl] [Stmt] SourcePos
  deriving (Show, Eq)

data Program =
  Program MainClass [ClassDecl] SourcePos
  deriving (Show, Eq)

data Expr
  = IntLit Integer SourcePos
  | Identifier' Identifier
  | BinaryOp Op Expr Expr SourcePos
  | True SourcePos
  | False SourcePos
  | This SourcePos
  | NewArray Size SourcePos
  | NewObject TypeName SourcePos
  | Parens Expr SourcePos
  | ArrayLength Expr SourcePos
  | MethodCall Expr Identifier ArgList SourcePos
  | ArrayLookup Expr Index SourcePos
  | Not Expr SourcePos
  deriving (Show, Eq)

exprPos :: Expr -> SourcePos
exprPos (IntLit _ pos) = pos
exprPos (Identifier' (Identifier _ pos)) = pos
exprPos (BinaryOp _ _ _ pos) = pos
exprPos (SyntaxTree.True pos) = pos
exprPos (SyntaxTree.False pos) = pos
exprPos (This pos) = pos
exprPos (NewArray _ pos) = pos
exprPos (NewObject _ pos) = pos
exprPos (Parens _ pos) = pos
exprPos (ArrayLength _ pos) = pos
exprPos (MethodCall _ _ _ pos) = pos
exprPos (ArrayLookup _ _ pos) = pos
exprPos (Not _ pos) = pos

data Identifier =
  Identifier String SourcePos
  deriving (Show, Eq)

data Op
  = Mult
  | Plus
  | Minus
  | LessThan
  | LessOrEqualThan
  | GreaterThan
  | GreaterOrEqualThan
  | Equal
  | NotEqual
  | And
  | Or
  deriving (Show, Eq)

data TypeNode
  = BooleanTypeNode SourcePos
  | IntArrayTypeNode SourcePos
  | IntTypeNode SourcePos
  | ObjectTypeNode String SourcePos
  deriving (Show, Eq)

data Stmt
  = ArrayAssign Assignee Index Expr SourcePos
  | Assign Assignee Expr SourcePos
  | Block [Stmt] SourcePos
  | If Expr Stmt Stmt SourcePos
  | IfWithoutElse Expr Stmt SourcePos
  | Syso Expr SourcePos
  | While Expr Stmt SourcePos
  deriving (Show, Eq)

data GenVarDecl =
  GenVarDecl TypeNode VarName VarDeclType SourcePos
  deriving (Show, Eq)

data VarDeclType
  = VarDecl
  | Formal
  deriving (Show, Eq)
