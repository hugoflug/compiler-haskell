module ErrorFormat
  ( formatError
  ) where

import Text.Parsec (SourcePos, sourceColumn, sourceLine)

import Errors
import SymbolTable (RedefinitionError(RedefinitionError))
import TypeCheck (TypeError(..))

formatError :: String -> CompilationError -> String
formatError sourceFile err =
  formatErrorType err ++ " at " ++ formatSourcePos (errorPos err) ++ " in " ++ sourceFile

formatErrorType :: CompilationError -> String
formatErrorType (ParseError' parseErr) = "Parse error: " ++ show parseErr
formatErrorType (TypeError' (MultiDimArrayError _)) =
  "Parse error: multidimensional arrays not allowed"
formatErrorType (RedefinitionError' (RedefinitionError name _)) =
  "Redefinition error: " ++ name ++ " already defined"
formatErrorType (TypeError' (WrongTypeError expected actual _)) =
  "Type error: Expected " ++ show expected ++ " but was " ++ show actual
formatErrorType (TypeError' (NotObjectTypeError actual _)) =
  "Type error: Expected an object type but was: " ++ show actual
formatErrorType (TypeError' (UndefinedNameError name _)) = "Undefined name: " ++ name
formatErrorType (TypeError' (WrongArgumentAmountError expected actual _)) =
  "Wrong number of arguments: Expected " ++ show expected ++ " but was " ++ show actual
formatErrorType (TypeError' (IntSizeError val _)) =
  "Number size error: " ++ show val ++ " is too large for an int"

formatSourcePos :: SourcePos -> String
formatSourcePos pos = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
