module Compile
  ( compile
  , compileFromFile
  , FileContent(FileContent)
  ) where

import Errors
import Parse
import SymbolTable

import Text.Parsec (errorPos)
import Text.Parsec.Pos (initialPos)

data FileContent =
  FileContent FilePath String

compileFromFile :: FilePath -> IO (Either CompilationError [FileContent])
compileFromFile filename = do
  code <- readFile filename
  return $ compile filename code

compile :: FilePath -> String -> Either CompilationError [FileContent]
compile filename code = do
  program <- parse filename code
  symTable <- mkSymTable program
  return [FileContent "" ""]
