module CTG1371Spec (spec) where

import CTG1371

import Test.Hspec

spec :: Spec
spec =
    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()
