{-# LANGUAGE NamedFieldPuns #-}

module CodeGen
  ( generate
  ) where

import Data.Hashable
import Data.Map ((!), size)
import Data.DList

import JVMByteCode
import SymbolTable
import SyntaxTree as AST
import Type
import TypeCheck

type Hash = Int

data Context =
  Context {
    symTable :: SymbolTable,
    classTable :: ClassTable, 
    methodTable :: MethodTable,
    cHash :: Hash
  }
  deriving (Eq, Show)

(+:) :: [a] -> a -> [a]
a +: b = a ++ [b]

generate :: SymbolTable -> Program -> [JVMClass]
generate symTable (Program mainClass classDecls _) =
  (genMainClass symTable mainClass) : (genClass symTable <$> classDecls)

genMainClass :: SymbolTable -> MainClass -> JVMClass
genMainClass symTable (MainClass (Identifier name _) _ fields stmts _) =
  JVMClass name "java/lang/Object" jvmFields [mainMethod]
  where
    jvmFields = genField <$> fields
    mainMethod = genMainMethod symTable classTable stmts
    classTable = symTable ! name

genClass :: SymbolTable -> ClassDecl -> JVMClass
genClass symTable (ClassDecl (Identifier name _) fields methods _) =
  JVMClass name "java/lang/Object" jvmFields jvmMethods
  where
    jvmFields = genField <$> fields
    jvmMethods = genMethod Static symTable classTable <$> methods
    classTable = symTable ! name

typeDescriptor :: Type -> TypeDescriptor
typeDescriptor (ObjectType name) = "L" ++ name ++ ";"
typeDescriptor IntType = "I"
typeDescriptor IntArrayType = "[I"
typeDescriptor BooleanType = "I"
typeDescriptor VoidType = "V"

methodTypeDescriptor :: [Type] -> Type -> TypeDescriptor
methodTypeDescriptor types returnType =
  "(" ++ show (typeDescriptor <$> types) ++ ")" ++ show (typeDescriptor returnType)

genField :: GenVarDecl -> JVMField
genField = undefined

-- TODO: code duplication
genMainMethod :: SymbolTable -> ClassTable -> [Stmt] -> JVMMethod
genMainMethod symTable classTable stmts =
  JVMMethod name "([Ljava/lang/String;)V" Static maxStack maxLocals code
  where
    name = "main"
    maxStack = calcStackDepth code symTable
    maxLocals = toInteger $ size (locals methodTable) + size (params methodTable)
    methodTable = (methods classTable) ! name
    code = genMainMethodCode symTable classTable stmts
    varDeclType (GenVarDecl t _ _ _) = t

genMainMethodCode :: SymbolTable -> ClassTable -> [Stmt] -> [JVMInstruction]
genMainMethodCode symTable class'@ClassTable{methods} stmts =
  let c = Context symTable class' (methods ! "main") (hash (0 :: Int))
   in genAllStmt c 0 stmts ++ [Return]

genMethod :: MethodKind -> SymbolTable -> ClassTable -> MethodDecl -> JVMMethod
genMethod kind symTable classTable method@(MethodDecl typeName (Identifier name _) argList _ _ _ _) =
  JVMMethod name typeDesc kind maxStack maxLocals code
  where
    typeDesc = methodTypeDescriptor (typeOfNode . varDeclType <$> argList) (typeOfNode typeName)
    maxStack = calcStackDepth code symTable
    maxLocals = let methodTable = (methods classTable) ! name
     in toInteger $ size (locals methodTable) + size (params methodTable)
    code = genCode symTable classTable method
    varDeclType (GenVarDecl t _ _ _) = t

calcStackDepth = undefined

genCode :: SymbolTable -> ClassTable -> MethodDecl -> [JVMInstruction]
genCode symTable class'@ClassTable{methods} (MethodDecl returnType (Identifier name _) _ _ stmts returnVal _) =
  let c = Context symTable class' (methods ! name) (hash (0 :: Int))
   in genAllStmt c 0 stmts ++ genExpr c 1 returnVal +: retInstr returnType
  where
    retInstr (IntTypeNode _) = Ireturn
    retInstr (BooleanTypeNode _) = Ireturn
    retInstr _ = Areturn

genBinaryOp :: Context -> JVMInstruction -> Expr -> Expr -> [JVMInstruction]
genBinaryOp c instr l r = genExpr c 0 l ++ genExpr c 1 r +: instr

genComparisonOp :: Context -> LabelInstruction -> Expr -> Expr -> [JVMInstruction]
genComparisonOp c@Context{cHash} compareInstr l r =
  let setTrue = cHash
      after = cHash + 1
   in genExpr c 0 l ++
      genExpr c 1 r +:
      Labelled compareInstr setTrue +:
      Iconst_0 +:
      Labelled Goto after +:
      Label setTrue +: 
      Iconst_1 +:
      Label after

genShortCircuitOp :: Context -> LabelInstruction -> Expr -> Expr -> [JVMInstruction]
genShortCircuitOp c@Context{cHash} compareInstr l r =
  let label = cHash
  in genExpr c 0 l +:
     Dup +:
     Labelled compareInstr label +:
     Pop ++
     genExpr c 1 r +:
     Label label


genExpr' :: Context -> Expr -> [JVMInstruction]
genExpr' c (BinaryOp Plus l r _) = genBinaryOp c Iadd l r
genExpr' c (BinaryOp Minus l r _) = genBinaryOp c Isub l r
genExpr' c (BinaryOp Mult l r _) = genBinaryOp c Imul l r
genExpr' c (BinaryOp GreaterThan l r _) = genComparisonOp c If_icmpgt l r
genExpr' c (BinaryOp LessThan l r _) = genComparisonOp c If_icmplt l r
genExpr' c (BinaryOp GreaterOrEqualThan l r _) = genComparisonOp c If_icmpge l r
genExpr' c (BinaryOp LessOrEqualThan l r _) = genComparisonOp c If_icmple l r
genExpr' c (BinaryOp Or l r _) = genShortCircuitOp c Ifne l r
genExpr' c (BinaryOp And l r _) = genShortCircuitOp c Ifeq l r 
genExpr' c (BinaryOp Equal _ r _) = 
  let compareInstr = case getType c r of
      ObjectType _ -> If_acmpeq
      IntArrayType -> If_acmpeq
      _ -> If_icmpne
  in genComparisonOp c compareInstr l r
genExpr' c (BinaryOp NotEqual _ r _) = 
  let compareInstr = case getType c r of
    ObjectType _-> If_acmpne
    IntArrayType -> If_acmpne
    _ -> If_icmpne
  in genComparisonOp c compareInstr l r
genExpr' c (IntLit val _) = [Ldc_wInt (fromInteger val)]
genExpr' c (Not expr _) = genExpr c 0 expr +: Iconst_1 +: Ixor
genExpr' c (NewArray arraySize _) = genExpr c 0 arraySize +: Newarray 10
genExpr' _ (NewObject (Identifier typeName _) _) = 
  [New typeName, Dup, MethodRefInstruction Invokespecial typeName "<init>" "()V"]
genExpr' c (ArrayLength array _) = genExpr c 0 array +: Arraylength
genExpr' c (ArrayLookup array index _) = genExpr c 0 array ++ genExpr c 1 index +: Iaload
genExpr' _ (AST.False _) = [Iconst_0]
genExpr' _ (AST.True _) = [Iconst_1]
genExpr' c (Parens expr _) = genExpr c 0 expr
genExpr' (Context _ ClassTable{className, fields} MethodTable{params, locals} _) (Identifier name _) = 
   case  locals !? name <|> params !? name of
     Just (Var _ IntType varNo) -> [Iload varNo]
     Just (Var _ BooleanType varNo) -> [Iload varNo]
     Just (Var _ _ varNo) -> [Aload varNo]
     Nothing -> 
      let typeDesc = typeDescriptor . varType . (!) fields $ name
        in [Aload_0, FieldRefInstruction Getfield className name typeDesc]





genStmt' :: Context -> Stmt -> [JVMInstruction]
genStmt' c@(Context{cHash}) (If condition then' else' _) =
  let label = cHash
      after = cHash
  in genExpr c 0 condition +:
     Labelled Ifeq label ++
     genStmt c 1 then' +:
     Labelled Goto after +:
     Label label ++
     genStmt c 2 else' +:
     Label after
genStmt' c@Context{cHash} (IfWithoutElse condition then' _) =
  let label = cHash
  in genExpr c 0 condition +:
     Labelled Ifeq label ++
     genStmt c 1 then' +:
     Label label

genExpr :: Context -> Int -> Expr -> [JVMInstruction]
genExpr = withHash genExpr'

genStmt :: Context -> Int -> Stmt -> [JVMInstruction]
genStmt = withHash genStmt'

genAllExpr :: Context -> Int -> [Expr] -> [JVMInstruction]
genAllExpr = genAll genExpr'

genAllStmt :: Context -> Int -> [Stmt] -> [JVMInstruction]
genAllStmt = genAll genStmt'

genAll :: (Context -> a -> [JVMInstruction]) -> Context -> Int -> [a] -> [JVMInstruction]
genAll gen c childNo =
   concatMap (\(i, n) -> gen (updateHash i c) n) . zip [childNo :: Int ..]

withHash :: (Context -> a -> [JVMInstruction]) -> Context -> Int -> a -> [JVMInstruction]
withHash gen c childNo node = gen (updateHash childNo c) node

updateHash :: Hashable a => a -> Context -> Context
updateHash i (Context st ct mt hash) = Context st ct mt (hashWithSalt hash i)
