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
  ) where

import Data.List (find)
import qualified Data.Map as M
import Text.Parsec.Pos (initialPos)

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
    }
  deriving (Show, Eq)

data RedefinitionError =
  RedefinitionError Name
  deriving (Show, Eq)

data MethodTable =
  MethodTable
    { methodName :: Name
    , returnType :: Type
    , params :: Params
    , locals :: Locals
    }
  deriving (Show, Eq)

data Var =
  Var
    { varName :: Name
    , varType :: Type
    , varNo :: VarNo
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

dedup :: Show k => M.Map k [v] -> Either RedefinitionError (M.Map k v)
dedup m = maybe (removeDups m) toError . find (\(_, l) -> length l > 1) . M.assocs $ m
  where
    removeDups m = Right $ fmap head m
    toError (k, _) = Left $ RedefinitionError (show k)

mkVar :: (Integer, GenVarDecl) -> Var
mkVar (index, (GenVarDecl typeNode (Identifier varName _) _ _)) =
  Var varName (typeOfNode typeNode) index

mkVarMap :: Integer -> [GenVarDecl] -> M.Map String [Var]
mkVarMap index = groupBy varName . fmap mkVar . zip [index ..]

mkMainClassTable :: MainClass -> Either RedefinitionError ClassTable
mkMainClassTable (MainClass (Identifier name _) _ varDecls _ _) = do
  varDecls <- dedup . mkVarMap 0 $ varDecls
  let methods = M.fromList [("main", (MethodTable "main" VoidType M.empty varDecls))]
  return $ ClassTable name methods M.empty

mkClassTable :: ClassDecl -> Either RedefinitionError ClassTable
mkClassTable (ClassDecl (Identifier name _) varDecls methodDecls _) = do
  fields <- dedup . mkVarMap 0 $ varDecls
  methodTables <- sequence . map mkMethodTable $ methodDecls
  methods <- dedup . groupBy methodName $ methodTables
  return $ ClassTable name methods fields

mkMethodTable :: MethodDecl -> Either RedefinitionError MethodTable
mkMethodTable (MethodDecl type' (Identifier name _) argList varDeclList _ _ _) = do
  params <- dedup . mkVarMap 0 $ argList
  let paramLen = toInteger . length $ params
  locals <- dedup . mkVarMap paramLen $ varDeclList
  dedup . groupBy id $ M.keys params ++ M.keys locals
  return $ MethodTable name (typeOfNode type') params locals

mkSymTable :: Program -> Either RedefinitionError SymbolTable
mkSymTable (Program mainClass classDecls _) = do
  classTables <- sequence . map mkClassTable $ classDecls
  mainClassTable <- mkMainClassTable mainClass
  dedup . groupBy className $ mainClassTable : classTables
