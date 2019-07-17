module Compile
    ( 
        compile,
        compileFromFile
    ) where

import Parse
import Errors
import Data.Either.Combinators

import Text.Parsec.Pos(initialPos)

compileFromFile :: FilePath -> IO (Either CompilationError [(FilePath, String)])
compileFromFile filename = do
    program <- readFile filename 
    return $ compile filename program

compile :: FilePath -> String -> Either CompilationError [(FilePath, String)]
compile filename program = do
    mapLeft (\_ -> CompilationError (initialPos "asd") (ParseError "")) $ parse filename program 
    return [("", "")]