module Errors where

import Text.Parsec (ParseError, SourcePos, errorPos)

import SymbolTable (RedefinitionError(), errorPos)
import TypeCheck (TypeError(), errorPos)

type VarName = String

type Message = String

data CompilationError
  = RedefinitionError' RedefinitionError
  | ParseError' ParseError
  | TypeError' TypeError
  deriving (Show, Eq)

errorPos :: CompilationError -> SourcePos
errorPos (RedefinitionError' redefError) = SymbolTable.errorPos redefError
errorPos (ParseError' parseErr) = Text.Parsec.errorPos parseErr
errorPos (TypeError' typeErr) = TypeCheck.errorPos typeErr
