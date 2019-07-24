import Control.Monad
import Data.Either.Combinators
import Data.List (intersect, isSuffixOf, partition)
import Data.Maybe (fromJust)
import Data.Text (pack, strip, stripPrefix, unpack)
import System.Directory
import Test.Hspec

import Text.Regex.TDFA

import Compile

testCaseDir = "test/integration/testcases/"

extensions = ["JVM", "IWE", "CLE", "CGT", "CGE", "CEQ", "CNE", "BDJ"]

strip' :: String -> String
strip' = unpack . strip . pack

mapFst :: Functor f => (a -> b) -> (f a, x) -> (f b, x)
mapFst f (a, b) = (fmap f a, b)

shouldSkip :: String -> Bool
shouldSkip program = unsupportedExt || incompatibleExt
  where
    unsupportedExt = length (intersect requiredExts extensions) < length requiredExts
    incompatibleExt = not . null . intersect forbiddenExts $ extensions
    (forbiddenExts, requiredExts) =
      mapFst tail . partition (elem '!') . map strip' . map (\l -> l !! 2) $
      program =~ "(^|\n)// *EXT:(.*)"

shouldTestFile :: FilePath -> IO Bool
shouldTestFile filename = readFile filename >>= return . not . shouldSkip

listDir :: FilePath -> IO [FilePath]
listDir dir = fmap (map (\file -> dir ++ "/" ++ file)) . listDirectory $ dir

getAllFiles :: String -> IO [FilePath]
getAllFiles topDir = do
  entries <- listDir $ testCaseDir ++ topDir
  subdirs <- filterM doesDirectoryExist entries
  files <- fmap concat . mapM listDir $ subdirs
  filterM shouldTestFile . filter (isSuffixOf ".java") $ files

stripPrefix' :: String -> String -> String
stripPrefix' prefix = unpack . fromJust . stripPrefix (pack prefix) . pack

assertResult assert filename = do
  specify (stripPrefix' testCaseDir filename) $ do
    result <- compileFromFile filename
    result `shouldSatisfy` assert

main = do
  compileFiles <- getAllFiles "compile"
  nonCompileFiles <- getAllFiles "noncompile"
  executeFiles <- getAllFiles "execute"
  nonExecuteFiles <- getAllFiles "nonexecute"
  hspec $
    context "Integration tests" $ do
      mapM_ (assertResult isRight) compileFiles
      mapM_ (assertResult isRight) executeFiles
      mapM_ (assertResult isRight) nonExecuteFiles
      mapM_ (assertResult isLeft) nonCompileFiles
