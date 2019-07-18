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
  deriving (Show, Eq)
