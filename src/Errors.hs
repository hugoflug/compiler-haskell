module Errors where

import Text.Parsec (SourcePos)

data CompilationError =
  CompilationError SourcePos ErrorInfo
  deriving (Show, Eq)

type VarName = String

type Message = String

data ErrorInfo
  = RedefinitionError VarName
  | ParseError Message
  deriving (Show, Eq)

redefError varName pos = CompilationError pos $ RedefinitionError varName
