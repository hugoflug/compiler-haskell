module ParseSpec
  ( spec
  ) where

import Data.Either.Combinators (isRight)
import Parse
import SyntaxTree as AST
import Test.Hspec

spec :: Spec
spec =
  describe "parse" $ do
    it "should parse simple expression correctly" $ do
      parse "" "true + false" `shouldSatisfy` isRight
