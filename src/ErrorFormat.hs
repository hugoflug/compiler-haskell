module ErrorFormat
    ( 
      format
    ) where

import Data.List.Split (splitOn)

import Text.Parsec(SourcePos, sourceLine, sourceColumn)

import Errors

format :: CompilationError -> String -> String -> String
format (CompilationError index errorInfo) program sourceFile =
    formatError errorInfo ++ " at " ++ formatSourcePos index program ++ " in " ++ sourceFile

formatError :: ErrorInfo -> String
formatError (RedefError name) = 
    "Redefinition error: " ++ name ++ " already defined"

formatSourcePos :: SourcePos -> String -> String
formatSourcePos pos program = 
    show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
