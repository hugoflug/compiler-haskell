module Errors where

data CompilationError = CompilationError { index :: Int, error :: ErrorInfo } deriving (Show, Eq)
data ErrorInfo =
    RedefError { name :: String } deriving (Show, Eq)
