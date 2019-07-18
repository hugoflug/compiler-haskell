module SyntaxTree where

import Text.Parsec

data SyntaxTreeNode
  = Expr Expr
  | TypeNode TypeNode
  | Stmt Stmt
  | GenVarDecl_ GenVarDecl
  | MethodDecl_ MethodDecl
  | ClassDecl_ ClassDecl
  | MainClass_ MainClass
  | Program_ Program
  deriving (Show, Eq)

data MethodDecl =
  MethodDecl
    { returnType :: TypeNode
    , name :: Expr
    , argList :: [GenVarDecl]
    , methodVarDecls :: [GenVarDecl]
    , methodStmts :: [Stmt]
    , returnVal :: Expr
    , methodDeclpos :: SourcePos
    }
  deriving (Show, Eq)

data ClassDecl =
  ClassDecl
    { className :: Expr
    , varDecls :: [GenVarDecl]
    , methodDecls :: [MethodDecl]
    , classDeclpos :: SourcePos
    }
  deriving (Show, Eq)

data MainClass =
  MainClass
    { mainClassName :: String
    , stdArgsName :: String
    , mainVarDecls :: [GenVarDecl]
    , stmts :: [Stmt]
    , mainClasspos :: SourcePos
    }
  deriving (Show, Eq)

data Program =
  Program
    { mainClass :: MainClass
    , classDecls :: [ClassDecl]
    , programpos :: SourcePos
    }
  deriving (Show, Eq)

data Expr
  = IntLit
      { value :: Integer
      , pos :: SourcePos
      }
  | Identifier_ Identifier
  | BinaryOp_ BinaryOp
  | True
      { pos :: SourcePos
      }
  | False
      { pos :: SourcePos
      }
  | This
      { pos :: SourcePos
      }
  | NewArray
      { arraySize :: Expr
      , pos :: SourcePos
      }
  | NewObject
      { typeName :: Expr
      , pos :: SourcePos
      }
  | Parens
      { expr :: Expr
      , pos :: SourcePos
      }
  | ArrayLength
      { array :: Expr
      , pos :: SourcePos
      }
  | MethodCall
      { obj :: Expr
      , methodName :: String
      , args :: [Expr]
      , pos :: SourcePos
      }
  | ArrayLookup
      { array :: Expr
      , arrayIndex :: Expr
      , pos :: SourcePos
      }
  | Not
      { expr :: Expr
      , pos :: SourcePos
      }
  deriving (Show, Eq)

data Identifier =
  Identifier
    { idName :: String
    , idpos :: SourcePos
    }
  deriving (Show, Eq)

data BinaryOp =
  BinaryOp
    { op :: Op
    , leftOp :: Expr
    , rightOp :: Expr
    , binOpIndex :: SourcePos
    }
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
  = BooleanTypeNode
      { typeNodepos :: SourcePos
      }
  | IntArrayTypeNode
      { typeNodepos :: SourcePos
      }
  | IntTypeNode
      { typeNodepos :: SourcePos
      }
  | ObjectTypeNode
      { objectName :: String
      , typeNodepos :: SourcePos
      }
  deriving (Show, Eq)

data Stmt
  = ArrayAssign
      { assignArray :: Expr
      , assignArrayIndex :: Expr
      , newValue :: Expr
      , stmtpos :: SourcePos
      }
  | Assign
      { assignee :: Expr
      , newValue :: Expr
      , stmtpos :: SourcePos
      }
  | Block
      { stmtList :: [Stmt]
      , stmtpos :: SourcePos
      }
  | If
      { ifCondition :: Expr
      , thenStmt :: Stmt
      , elseStmt :: Stmt
      , stmtpos :: SourcePos
      }
  | IfWithoutElse
      { iweCondition :: Expr
      , thenStmt :: Stmt
      , stmtpos :: SourcePos
      }
  | Syso
      { printee :: Expr
      , stmtpos :: SourcePos
      }
  | While
      { whileCondition :: Expr
      , whileStmt :: Stmt
      , stmtpos :: SourcePos
      }
  deriving (Show, Eq)

data GenVarDecl =
  GenVarDecl
    { typeNode :: TypeNode
    , varName :: Expr
    , var :: VarDeclType
    , genVarDeclpos :: SourcePos
    }
  deriving (Show, Eq)

data VarDeclType
  = VarDecl
  | Formal
  deriving (Show, Eq)
