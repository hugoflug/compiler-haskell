module ErrorFormat
  ( formatError
  ) where

import Data.List.Split (splitOn)

import Text.Parsec (SourcePos, sourceColumn, sourceLine)

import Errors

formatError :: String -> CompilationError -> String
formatError sourceFile (CompilationError index errorInfo) =
  formatErrorInfo errorInfo ++ " at " ++ formatSourcePos index ++ " in " ++ sourceFile

formatErrorInfo :: ErrorInfo -> String
formatErrorInfo (RedefinitionError name) = "Redefinition error: " ++ name ++ " already defined"
formatErrorInfo (ParseError message) = "Parse error: " ++ message

formatSourcePos :: SourcePos -> String
formatSourcePos pos = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
