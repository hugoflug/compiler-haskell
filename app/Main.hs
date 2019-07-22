module Main where

import Data.Either.Combinators (mapLeft)

import System.Environment

import Compile
import ErrorFormat
import Errors

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: mjc <file> [<file> ...]"
    else do
      results <- mapM compileFromFile args
      handleResult . sequence $ zipLeft args results
  where
    handleResult (Left (filePath, error)) = print . formatError filePath $ error
    handleResult (Right results) = sequence_ . map writeFileContent . concat $ results

zipLeft :: [a] -> [Either l r] -> [Either (a, l) r]
zipLeft l1 l2 = map intoLeft $ zip l1 l2
  where
    intoLeft (a, Left b) = Left (a, b)
    intoLeft (a, Right x) = Right x

writeFileContent :: FileContent -> IO ()
writeFileContent (FileContent filePath content) = writeFile filePath content
