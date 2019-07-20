module Compile
  ( compile
  , compileFromFile
  ) where

import Data.Either.Combinators
import Errors
import Parse
import SymbolTable

import Text.Parsec (errorPos)
import Text.Parsec.Pos (initialPos)

compileFromFile :: FilePath -> IO (Either CompilationError [(FilePath, String)])
compileFromFile filename = do
  program <- readFile filename
  return $ compile filename program

compile :: FilePath -> String -> Either CompilationError [(FilePath, String)]
compile filename program = do
  program <- parse filename program
  symTable <- mkSymTable program
  return [("", "")]
