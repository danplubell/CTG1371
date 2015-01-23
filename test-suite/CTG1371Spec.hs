module CTG1371Spec (spec) where

import Data.CTG1371.Parser
import Control.Exception (evaluate)
import Data.ByteString

import Test.Hspec

testDataNoCBlock :: ByteString
testDataNoCBlock = pack [66, -128, 0, 33, 113, 33, 113, 33, 113, 33, 113, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34, 2, 0, 0] 

testDataFMPEnabled::ByteString
testDataFMPEnabled = pack [67, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34, 2, 0, 0]

spec :: Spec
spec =
    describe "Parse CTG data" $ do
        it "is a valid C block" $
            evaluate (ctgParser testDataNoCBlock) `shouldThrow` anyException
        it "FMP is enabled" $ do
            fmpEnabled (ctgStatus (ctgParser testDataFMPEnabled)) `shouldBe` True
