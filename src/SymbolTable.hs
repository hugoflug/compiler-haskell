module SymbolTable
    ( 
      
    ) where

import Data.Map

import Type
import Errors
import SyntaxTree

type SymbolTable = Map String ClassTable

data ClassTable = ClassTable { className :: String, methods :: Map String MethodTable, fields :: Map String Var }
data MethodTable = MethodTable { methodName :: String, returnType :: Type, params :: Map String Var, locals :: Map String Var }
data Var = Var { varName :: String, type_ :: Type, varNo :: Int }

mkSymTable :: Program -> Either CompilationError SymbolTable
mkSymTable (Program mainClass classDecls _) = undefined

