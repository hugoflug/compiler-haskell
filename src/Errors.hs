module Errors where

import Text.Parsec (SourcePos)

data CompilationError =
  CompilationError
    { pos :: SourcePos
    , error :: ErrorInfo
    }
  deriving (Show, Eq)

data ErrorInfo
  = RedefError
      { name :: String
      }
  | ParseError
      { message :: String
      }
  | RedefinitionError String
  deriving (Show, Eq)

redefError varName pos = CompilationError pos $ RedefinitionError varName
