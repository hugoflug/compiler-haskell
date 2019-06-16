module ParseSpec(spec) where

import Test.Hspec
import Parse
import SyntaxTree as AST

spec :: Spec
spec =
    describe "parse" $ do
        it "should parse simple expression correctly" $ do
             parse "" "true + false" `shouldBe` Right (BinaryOp_ (BinaryOp Plus (AST.True 0) (AST.False 0) 0))
