module Compile
    ( 
        compile,
        compileFromFile
    ) where

import Parse
import Errors
import Data.Either.Combinators

compileFromFile :: FilePath -> IO (Either CompilationError [(FilePath, String)])
compileFromFile filename = do
    program <- readFile filename 
    return $ compile filename program

compile :: FilePath -> String -> Either CompilationError [(FilePath, String)]
compile filename program = do
    mapLeft (\_ -> ParseError "asd") parse filename program 
    return [("", "")]