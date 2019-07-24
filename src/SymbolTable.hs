module SymbolTable
  ( mkSymTable
  , SymbolTable
  , ClassTable(ClassTable)
  , MethodTable(MethodTable)
  , Var(Var)
  , RedefinitionError(RedefinitionError)
  , className
  , methods
  , fields
  , methodName
  , returnType
  , params
  , locals
  , varName
  , varType
  , varNo
  , errorPos
  ) where

import Data.List (find)
import qualified Data.Map as M
import Text.Parsec.Pos (SourcePos)

import SyntaxTree
import Type

type SymbolTable = M.Map String ClassTable

type VarNo = Integer

type Params = M.Map String Var

type Locals = M.Map String Var

type Methods = M.Map String MethodTable

type Fields = M.Map String Var

type Name = String

data ClassTable =
  ClassTable
    { className :: Name
    , methods :: Methods
    , fields :: Fields
    , classPos :: SourcePos
    }
  deriving (Show, Eq)

data RedefinitionError =
  RedefinitionError Name SourcePos
  deriving (Show, Eq)

errorPos :: RedefinitionError -> SourcePos
errorPos (RedefinitionError _ pos) = pos

data MethodTable =
  MethodTable
    { methodName :: Name
    , returnType :: Type
    , params :: Params
    , locals :: Locals
    , methodPos :: SourcePos
    }
  deriving (Show, Eq)

data Var =
  Var
    { varName :: Name
    , varType :: Type
    , varNo :: VarNo
    , varPos :: SourcePos
    }
  deriving (Show, Eq)

typeOfNode :: TypeNode -> Type
typeOfNode (BooleanTypeNode _) = BooleanType
typeOfNode (IntTypeNode _) = IntType
typeOfNode (IntArrayTypeNode _) = IntArrayType
typeOfNode (ObjectTypeNode typeName _) = ObjectType typeName

groupBy :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupBy key as = M.fromListWith (++) as'
  where
    as' = fmap ((,) <$> key <*> (: [])) as

dedup :: Show k => (v -> SourcePos) -> M.Map k [v] -> Either RedefinitionError (M.Map k v)
dedup getPos m = maybe (removeDups m) toError . find (\(_, l) -> length l > 1) . M.assocs $ m
  where
    removeDups m = Right . fmap head $ m
    toError (k, val) = Left . RedefinitionError (show k) . getPos . head $ val

dedupVars :: M.Map Name [Var] -> Either RedefinitionError (M.Map Name Var)
dedupVars = dedup varPos

dedupClasses :: M.Map Name [ClassTable] -> Either RedefinitionError (M.Map Name ClassTable)
dedupClasses = dedup classPos

dedupMethods :: M.Map Name [MethodTable] -> Either RedefinitionError (M.Map Name MethodTable)
dedupMethods = dedup methodPos

mkVar :: (Integer, GenVarDecl) -> Var
mkVar (index, (GenVarDecl typeNode (Identifier varName pos) _ _)) =
  Var varName (typeOfNode typeNode) index pos

mkVarMap :: Integer -> [GenVarDecl] -> M.Map String [Var]
mkVarMap index = groupBy varName . fmap mkVar . zip [index ..]

-- TODO: Set the actual position of the main method
mkMainClassTable :: MainClass -> Either RedefinitionError ClassTable
mkMainClassTable (MainClass (Identifier name pos) _ varDecls _ _) = do
  varDecls <- dedupVars . mkVarMap 0 $ varDecls
  let methods = M.fromList [("main", (MethodTable "main" VoidType M.empty varDecls pos))]
  return $ ClassTable name methods M.empty pos

mkClassTable :: ClassDecl -> Either RedefinitionError ClassTable
mkClassTable (ClassDecl (Identifier name pos) varDecls methodDecls _) = do
  fields <- dedupVars . mkVarMap 0 $ varDecls
  methodTables <- sequence . map mkMethodTable $ methodDecls
  methods <- dedupMethods . groupBy methodName $ methodTables
  return $ ClassTable name methods fields pos

mkMethodTable :: MethodDecl -> Either RedefinitionError MethodTable
mkMethodTable (MethodDecl type' (Identifier name pos) argList varDeclList _ _ _) = do
  params <- dedupVars . mkVarMap 0 $ argList
  let paramLen = toInteger . length $ params
  locals <- dedupVars . mkVarMap paramLen $ varDeclList
  dedupVars . groupBy varName $ M.elems params ++ M.elems locals
  return $ MethodTable name (typeOfNode type') params locals pos

mkSymTable :: Program -> Either RedefinitionError SymbolTable
mkSymTable (Program mainClass classDecls _) = do
  classTables <- sequence . map mkClassTable $ classDecls
  mainClassTable <- mkMainClassTable mainClass
  dedupClasses . groupBy className $ mainClassTable : classTables
