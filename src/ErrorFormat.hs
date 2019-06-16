module ErrorFormat
    ( 
      format
    ) where

import Data.List.Split (splitOn)

import Errors

format :: CompilationError -> String -> String -> String
format (CompilationError index errorInfo) program sourceFile =
    formatError errorInfo ++ " at " ++ formatSourcePos index program ++ " in " ++ sourceFile

formatError :: ErrorInfo -> String
formatError (RedefError name) = 
    "Redefinition error: " ++ name ++ " already defined"

formatSourcePos :: Int -> String -> String
formatSourcePos index program = 
    show row ++ ":" ++ show column
    where SourcePosition row column = toSourcePos program index 

data SourcePosition = SourcePosition { row :: Int, column :: Int }

toSourcePos :: String -> Int -> SourcePosition
toSourcePos program index = 
    toSourcePos_ lineLengths (index + 1) 1
    where
        lineLengths = fmap length . splitOn "\n" $ program 

toSourcePos_ :: [Int] -> Int -> Int -> SourcePosition
toSourcePos_ lineLengths index lines =
    if index <= head lineLengths then SourcePosition lines index
    else toSourcePos_ (tail lineLengths) (index - head lineLengths - 1) (lines + 1)
