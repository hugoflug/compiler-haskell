module SyntaxTree where

import Text.Parsec

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

data SyntaxTreeNode
  = Expr Expr
  | TypeNode TypeNode
  | Stmt Stmt
  | GenVarDecl' GenVarDecl
  | MethodDecl' MethodDecl
  | ClassDecl' ClassDecl
  | MainClass' MainClass
  | Program' Program
  deriving (Show, Eq)

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
  | BinaryOp' BinaryOp
  | True SourcePos
  | False SourcePos
  | This SourcePos
  | NewArray Size SourcePos
  | NewObject TypeName SourcePos
  | Parens Expr SourcePos
  | ArrayLength Expr SourcePos
  | MethodCall Expr Method ArgList SourcePos
  | ArrayLookup Expr Index SourcePos
  | Not Expr SourcePos
  deriving (Show, Eq)

data Identifier =
  Identifier String SourcePos
  deriving (Show, Eq)

data BinaryOp =
  BinaryOp Op Expr Expr SourcePos
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
