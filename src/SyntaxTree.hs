module SyntaxTree where

data SyntaxTreeNode = 
    Expr Expr |
    TypeNode TypeNode |
    Stmt Stmt |
    GenVarDecl_ GenVarDecl |
    MethodDecl_ MethodDecl |
    ClassDecl_ ClassDecl |
    MainClass_ MainClass |
    Program_ Program
    deriving (Show, Eq)

data MethodDecl = MethodDecl { returnType :: TypeNode, name :: Expr, argList :: [GenVarDecl], returnVal :: Expr, methodDeclIndex :: Int } deriving (Show, Eq)
data ClassDecl = ClassDecl { className :: Expr, varDecls :: [GenVarDecl], methodDecls :: [MethodDecl], classDeclIndex :: Int } deriving (Show, Eq)
data MainClass = MainClass { mainClassName :: Identifier, stdArgsName :: Identifier, mainVarDecls :: [GenVarDecl], stmts :: [Stmt], mainClassIndex :: Int } deriving (Show, Eq)
data Program = Program { mainClass :: MainClass, classDecls :: [ClassDecl], programIndex :: Int } deriving (Show, Eq)

data Expr = 
    IntLit { value :: Int, index :: Int} |
    Identifier_ Identifier |
    BinaryOp_ BinaryOp |
    True { index :: Int } |
    False { index :: Int } |
    This { index :: Int } |
    NewArray { arraySize :: Int, index :: Int } |
    NewObject { typeName :: Expr, index :: Int } |
    Parens { expr :: Expr, index :: Int } |
    ArrayLength { array :: Expr, index :: Int } |
    MethodCall { obj :: Expr, methodName :: Expr, args :: [Expr], index :: Int } |
    ArrayLookup { array :: Expr, arrayIndex :: Expr, index :: Int } |
    Not { expr :: Expr, index :: Int }
    deriving (Show, Eq)

data Identifier = Identifier { idName :: String, idIndex :: Int } deriving (Show, Eq)

data BinaryOp = BinaryOp { op :: Op, leftOp :: Expr, rightOp :: Expr, binOpIndex :: Int } deriving (Show, Eq)

data Op = 
    Mult |
    Plus |
    Minus |
    LessThan |
    LessOrEqualThan |
    GreaterThan |
    GreaterOrEqualThan |
    Equal |
    NotEqual |
    And |
    Or
    deriving (Show, Eq)

data TypeNode = 
    BooleanTypeNode { typeNodeIndex :: Int } |
    IntArrayTypeNode { typeNodeIndex :: Int } |
    IntTypeNode { typeNodeIndex :: Int }|
    ObjectTypeNode { objectName :: String, typeNodeIndex :: Int }
    deriving (Show, Eq)

data Stmt = 
    ArrayAssign { assignArray :: Expr, assignArrayIndex :: Expr, newValue :: Expr, stmtIndex :: Int } |
    Assign { assignee :: Expr, newValue :: Expr, stmtIndex :: Int  } |
    Block { stmtList :: [Stmt], stmtIndex :: Int } |
    If { ifCondition :: Expr, thenStmt :: Stmt, elseStmt :: Stmt, stmtIndex :: Int } |
    IfWithoutElse { iweCondition :: Expr, thenStmt :: Stmt, stmtIndex :: Int } |
    Syso { printee :: Expr, stmtIndex :: Int } |
    While { whileCondition :: Expr, whileStmt :: Stmt, stmtIndex :: Int }
    deriving (Show, Eq)

data GenVarDecl = GenVarDecl { typeNode :: TypeNode, varName :: Expr, var :: VarDeclType, genVarDeclIndex :: Int } deriving (Show, Eq)
data VarDeclType =
    VarDecl |
    Formal
    deriving (Show, Eq)
