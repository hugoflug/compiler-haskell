module Compile
  ( compile
  , compileFromFile
  , FileContent(FileContent)
  ) where

import Errors
import qualified Parse as P
import qualified SymbolTable as S
import SyntaxTree
import qualified TypeCheck as T

import Data.Either.Combinators (mapLeft, maybeToLeft)

data FileContent =
  FileContent FilePath String
  deriving (Eq, Show)

compileFromFile :: FilePath -> IO (Either CompilationError [FileContent])
compileFromFile filename = do
  code <- readFile filename
  return $ compile filename code

compile :: FilePath -> String -> Either CompilationError [FileContent]
compile filename code = do
  program <- parse filename code
  symTable <- mkSymTable program
  typeCheck symTable program
  return [FileContent "" ""]

parse :: FilePath -> String -> Either CompilationError Program
parse filename = mapLeft ParseError' . P.parse filename

mkSymTable :: Program -> Either CompilationError S.SymbolTable
mkSymTable = mapLeft RedefinitionError' . S.mkSymTable

typeCheck :: S.SymbolTable -> Program -> Either CompilationError ()
typeCheck symTable = maybeToLeft () . fmap TypeError' . T.typeCheck symTable
