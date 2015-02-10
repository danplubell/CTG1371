module CTG1371Spec (spec) where

--import Data.CTG1371.Internal.Encoder.Encoders
import Data.CTG1371.Internal.Parser.Parsers
--import Data.CTG1371.Encoder
import Data.CTG1371.Parser
import Control.Exception (evaluate)
import Data.ByteString
import Data.CTG1371.Internal.Types

import Test.Hspec


spec :: Spec
spec =
  describe "Parse CTG Data" $ do
    describe "Check CTG Header " $ do
        it "is a valid C block" $ do
            evaluate (ctgParser testDataNoCBlock) `shouldThrow` anyException

    describe "Check Status Block" $ do
        it "FMP is enabled" $ do 
           getFMPStatus  ( getCTGStatus (ctgParser testDataFMPEnabled)) `shouldBe` True
            
        it "FMP is disabled" $ do
            getFMPStatus ( getCTGStatus (ctgParser testDataAllOff)) `shouldBe` False

        it "TwinOffset is enabled" $ do
            getHRTwinOffsetStatus (getCTGStatus (ctgParser testDataHrTwinOn)) `shouldBe` True

        it "TwinOffset is disabled" $ do
            getHRTwinOffsetStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "DECG Logic is enabled" $ do
            getDECGLogicStatus (getCTGStatus (ctgParser testDataDECGOn)) `shouldBe` True

        it "DECG is disabled" $ do
            getDECGLogicStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "HR Cross Channel Verification is enabled" $ do
            getHRCrossChannelStatus (getCTGStatus ( ctgParser testDataHRCrossOn)) `shouldBe` True

        it "HR Cross Channel Verification is disabled" $ do
            getHRCrossChannelStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "Telemetry is enabled" $ do
            getTelemetryStatus (getCTGStatus ( ctgParser testDataTelemetryOn)) `shouldBe` True

        it "Telemetry is disabled" $ do
            getTelemetryStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "fspo2 is enabled" $ do
            getFSPO2AvailableStatus (getCTGStatus ( ctgParser testDataFspo2Available)) `shouldBe` True

        it "fspo2 is disabled" $ do
            getFSPO2AvailableStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "CTG data deleted is enabled" $ do
            getDataDeletedStatus (getCTGStatus ( ctgParser testDataCTGDataDeletedOn)) `shouldBe` True

        it "CTG data deleted is disabled" $ do
            getDataDeletedStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "CTG data inserted is enabled" $ do
            getDataInsertedStatus (getCTGStatus ( ctgParser testDataCTGDataInsertedOn)) `shouldBe` True

        it "CTG data inserted is disabled" $ do
            getDataInsertedStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "Monitor On is enabled" $ do
            getMonitorOnStatus (getCTGStatus ( ctgParser testDataMonitorOn)) `shouldBe` True

        it "Monitor On is disabled" $ do
            getMonitorOnStatus (getCTGStatus ( ctgParser testDataAllOff)) `shouldBe` False

        it "HR is equal" $ do
            buildHR 0 False SignalRed `shouldBe` 0 

    describe "test modes" $ do
      it "TOCO Mode NoTocoTransduder" $ do
        unpackTocoMode 0 `shouldBe` NoTOCOTransducer

      it "TOCO Mode ExternalTOCO" $ do
        unpackTocoMode 8 `shouldBe` ExternalTOCO

      it "TOCO Mode IUP" $ do
        unpackTocoMode 10 `shouldBe` IUP

      it "TOCO Mode Unknown TOCOMode" $ do
        unpackTocoMode 14 `shouldBe` UnknownTOCOMode

      it "MHRMode no transducer" $ do
        unpackMHRMode 0 `shouldBe` MHRNoHRTransducer

      it "MHRMode Inop" $ do
        unpackMHRMode 16 `shouldBe` MHRInop

      it "MHRMode MECG" $ do
        unpackMHRMode 96 `shouldBe` MECG

      it "MHRMode external mhr" $ do
        unpackMHRMode 128 `shouldBe` ExternalMHR

      it "MHRMode Reserved1" $ do
        unpackMHRMode 160 `shouldBe` MHRReserved1

      it "MHRMode Reserved2" $ do
        unpackMHRMode 192 `shouldBe` MHRReserved2

      it "HR1Mode No HR Transducer" $ do
        unpackHR1Mode 0 `shouldBe` NoHRTransducer

      it "HR1Mode Inop" $ do
        unpackHR1Mode 4096 `shouldBe` Inop

      it "HR1Mode US" $ do
        unpackHR1Mode 8192 `shouldBe` US

      it "HR1Mode DECG" $ do
        unpackHR1Mode 16384 `shouldBe` DECG

      it "HR1Mode Reserved2" $ do
        unpackHR1Mode 49152 `shouldBe` Reserved2

      it "HR1Mode UnknownHRMode" $ do
        unpackHR1Mode 57344 `shouldBe` UnknownHRMode

      it "HR2Mode No HR Transducer" $ do
        unpackHR2Mode 0 `shouldBe` NoHRTransducer

      it "HR2Mode Inop" $ do
        unpackHR2Mode 256 `shouldBe` Inop

      it "HR2Mode US" $ do
        unpackHR2Mode 512 `shouldBe` US

      it "HR2Mode DECG" $ do
        unpackHR2Mode 1024 `shouldBe` DECG

      it "HR2Mode Reserved2" $ do
        unpackHR2Mode 3072 `shouldBe` Reserved2

      it "HR2Mode Unknown mode" $ do
        unpackHR2Mode 3584 `shouldBe` UnknownHRMode

      
        
buildHR ::Int -> Bool -> SignalQuality -> Int
buildHR rate movement quality = rate * 4 + calcMovement + calcQuality
  where calcMovement = if movement then 0x800 else 0
        calcQuality = case quality of
                        SignalRed    -> 0
                        SignalYellow -> 0x2000
                        SignalGreen  -> 0x4000
  
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
