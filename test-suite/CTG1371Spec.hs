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

        it "HR Cross Channel Verification is enabled" $ do
            hrCrossChannelVer (ctgStatus ( ctgParser testDataHRCrossOn)) `shouldBe` True

        it "HR Cross Channel Verification is disabled" $ do
            hrCrossChannelVer (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "Telemetry is enabled" $ do
            telemetryOn (ctgStatus ( ctgParser testDataTelemetryOn)) `shouldBe` True

        it "Telemetry is disabled" $ do
            telemetryOn (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "fspo2 is enabled" $ do
            fspo2Available (ctgStatus ( ctgParser testDataFspo2Available)) `shouldBe` True

        it "fspo2 is disabled" $ do
            fspo2Available (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "CTG data deleted is enabled" $ do
            ctgDataDeleted (ctgStatus ( ctgParser testDataCTGDataDeletedOn)) `shouldBe` True

        it "CTG data deleted is disabled" $ do
            ctgDataDeleted (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "CTG data inserted is enabled" $ do
            ctgDataInserted (ctgStatus ( ctgParser testDataCTGDataInsertedOn)) `shouldBe` True

        it "CTG data inserted is disabled" $ do
            ctgDataInserted (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "Monitor On is enabled" $ do
            monitorOn (ctgStatus ( ctgParser testDataMonitorOn)) `shouldBe` True

        it "Monitor On is disabled" $ do
            monitorOn (ctgStatus ( ctgParser testDataAllOff)) `shouldBe` False


testDataMonitorOn::ByteString
testDataMonitorOn =  pack [0x43, 0x80 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataCTGDataInsertedOn::ByteString
testDataCTGDataInsertedOn =  pack [0x43, 0x40 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataCTGDataDeletedOn::ByteString
testDataCTGDataDeletedOn =  pack [0x43, 0x20 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataFspo2Available::ByteString
testDataFspo2Available =  pack [0x43, 0x8 , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataTelemetryOn::ByteString
testDataTelemetryOn =  pack [0x43, 0x2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataHRCrossOn::ByteString
testDataHRCrossOn =  pack [0x43, 0x1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataDECGOn::ByteString
testDataDECGOn =  pack [0x43, 0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataHrTwinOn::ByteString
testDataHrTwinOn =  pack [0x43, 0, 0x2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


testDataNoCBlock :: ByteString
testDataNoCBlock = pack [0x42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 

testDataFMPEnabled::ByteString
testDataFMPEnabled = pack [0x43, 0, 0x1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testDataAllOff::ByteString
testDataAllOff = pack [0x43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
