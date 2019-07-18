import Control.Monad
import Data.Either.Combinators
import Data.List (intersect, partition)
import Data.Maybe (fromJust)
import Data.Text (find, pack, strip, stripPrefix, unpack)
import System.Directory
import Test.Hspec

import Text.Regex.TDFA

import Compile

testCaseDir = "test/integration/testcases/"

extensions = ["JVM", "IWE", "CLE", "CGT", "CGE", "CEQ", "CNE", "BDJ"]

sStrip = unpack . strip . pack

shouldSkip :: String -> Bool
shouldSkip program = unsupportedExt || incompatibleExt
  where
    unsupportedExt = length (intersect requiredExts extensions) < length requiredExts
    incompatibleExt = not $ null $ intersect forbiddenExts extensions
    (forbiddenExts, requiredExts) = partition (elem '!') . map sStrip $ extInfos
    extInfos = map (\l -> l !! 2) $ program =~ "(^|\n)// *EXT:(.*)"

shouldTestFile :: FilePath -> IO Bool
shouldTestFile filename = readFile filename >>= \contents -> return $ not $ shouldSkip contents

getAllFiles :: String -> IO [FilePath]
getAllFiles topDir = do
  let activeDir = testCaseDir ++ topDir
  entries <- listDirectory activeDir
  subdirs <- filterM doesDirectoryExist . map (\a -> activeDir ++ "/" ++ a) $ entries
  files <- mapM listDirectory $ subdirs
  let dirsWithFiles = zip subdirs files
  let format dir file = dir ++ "/" ++ file
  let allFiles = concatMap (\(dir, files) -> map (format dir) files) dirsWithFiles
  filterM shouldTestFile allFiles

assertResult assert filename = do
  let stripDir = unpack . fromJust . stripPrefix (pack testCaseDir) . pack
  specify (stripDir filename) $ do
    result <- compileFromFile filename
    result `shouldSatisfy` assert

main = do
  files <- getAllFiles "compile"
  hspec $ context "Integration tests" $ mapM_ (assertResult isRight) files
