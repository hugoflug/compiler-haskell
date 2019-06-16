import System.Directory
import Control.Monad
import Test.Hspec
import Data.Either.Combinators
import Data.Text(stripPrefix, pack, unpack)
import Data.Maybe(fromJust)

import Compile

testCaseDir = "test/integration/testcases/"

getAllFiles :: String -> IO [String]
getAllFiles topDir = do
    let activeDir = testCaseDir ++ topDir
    entries <- listDirectory activeDir
    subdirs <- filterM doesDirectoryExist . map (\a -> activeDir ++ "/" ++ a) $ entries

    files <- mapM listDirectory $ subdirs
    let dirsWithFiles = zip subdirs files
    let format dir file = activeDir ++ "/" ++ dir ++ "/" ++ file
    return $ concatMap (\(dir, files) -> map (format dir) files) dirsWithFiles

assertResult assert filename = do 
    let stripDir = unpack . fromJust . stripPrefix (pack testCaseDir) . pack
    specify (stripDir filename) $ do
        result <- compileFromFile filename
        result `shouldSatisfy` assert

main = do
    files <- getAllFiles "compile"
    hspec $ context "Integration tests" $ mapM_ (assertResult isRight) files

    