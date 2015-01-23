module CTG1371Spec (spec) where

import Data.CTG1371.Parser
import Control.Exception (evaluate)
import Data.ByteString

import Test.Hspec


spec :: Spec
spec =
  describe "Parse CTG Data" $ do
    describe "Check CTG Header " $ do
        it "is a valid C block" $ do
            evaluate (ctgParser testDataNoCBlock) `shouldThrow` anyException

    describe "Check Status Block" $ do
        it "FMP is enabled" $ do 
            fmpEnabled ( ctgStatus (ctgParser testDataFMPEnabled)) `shouldBe` True
            
        it "FMP is disabled" $ do
            fmpEnabled ( ctgStatus (ctgParser testDataAllOff)) `shouldBe` False

        it "TwinOffset is enabled" $ do
            hrTwinOffsetOn (ctgStatus (ctgParser testDataHrTwinOn)) `shouldBe` True

        it "TwinOffset is disabled" $ do
            hrTwinOffsetOn (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "DECG Logic is enabled" $ do
            decgLogicOn (ctgStatus (ctgParser testDataDECGOn)) `shouldBe` True

        it "DECG is disabled" $ do
            decgLogicOn (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

testDataDECGOn::ByteString
testDataDECGOn =  pack [67, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataHrTwinOn::ByteString
testDataHrTwinOn =  pack [67, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataNoCBlock :: ByteString
testDataNoCBlock = pack [66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 

testDataFMPEnabled::ByteString
testDataFMPEnabled = pack [67, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataAllOff::ByteString
testDataAllOff = pack [67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
