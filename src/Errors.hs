module Errors where

import Text.Parsec (ParseError, SourcePos)
import Text.Parsec.Pos (initialPos)

import SymbolTable (RedefinitionError(RedefinitionError))
import TypeCheck (TypeError())

data CompilationError =
  CompilationError SourcePos ErrorInfo
  deriving (Show, Eq)

type VarName = String

type Message = String

data ErrorInfo
  = RedefinitionError' RedefinitionError
  | ParseError' ParseError
  | TypeError' TypeError
  deriving (Show, Eq)

redefError err = CompilationError (initialPos "tmp") $ RedefinitionError' $ err

typeError err = CompilationError (initialPos "tmp") $ TypeError' err

parseError err = CompilationError (initialPos "tmp") $ ParseError' err
