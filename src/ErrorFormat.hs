module ErrorFormat
  ( formatError
  ) where

import Data.List.Split (splitOn)

import Text.Parsec (SourcePos, sourceColumn, sourceLine)

import Errors
import SymbolTable (RedefinitionError(RedefinitionError))

formatError :: String -> CompilationError -> String
formatError sourceFile (CompilationError index errorInfo) =
  formatErrorInfo errorInfo ++ " at " ++ formatSourcePos index ++ " in " ++ sourceFile

formatErrorInfo :: ErrorInfo -> String
formatErrorInfo (RedefinitionError' (RedefinitionError name)) =
  "Redefinition error: " ++ name ++ " already defined"

-- formatErrorInfo (ParseError' (ParseError message)) = "Parse error: " ++ message
formatSourcePos :: SourcePos -> String
formatSourcePos pos = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
