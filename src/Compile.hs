module Compile
    ( 
        compile,
        compileFromFile
    ) where

import Parse
import Errors

compileFromFile :: FilePath -> IO (Either CompilationError [(FilePath, String)])
compileFromFile filename = return $ Right [("foo", "bar")]

compile = parse