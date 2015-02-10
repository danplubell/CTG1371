module EncoderSpec  where

import Test.Hspec
import Data.CTG1371.Encoder
import Data.CTG1371.Parser
import Data.CTG1371.Internal.Encoder.Encoders
import Data.CTG1371.Internal.Types
import Data.Word
import Data.Char
import Data.List (foldl', unfoldr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

binToWord16 :: String -> Word16
binToWord16 = foldl' (\acc x -> acc * 2 + digToWord16 x) 0
  where digToWord16 c = case c of
                          '0' -> 0
                          '1' -> 1
                          _   -> error "Invalid digit"

spec ::Spec
spec = describe "Test Encoder Internal Functions" $ do
         it "test packWord8 all true" $ do
           packWord8 (True,True,True,True,True,True,True,True) `shouldBe` 0xFF
           
         it "test packWord8 all false" $ do
           packWord8 (False,False,False,False,False,False,False,False) `shouldBe` 0

         it "test encodeHR for hr=100 fm == movement and Green signal quality" $ do
           encodeHR 100 Movement SignalGreen `shouldBe` binToWord16 "0100100110010000"

         it "test encodeHR hr = 256 fm = no movement and Red signal quality" $ do
           encodeHR 256 NoMovement SignalRed `shouldBe` binToWord16 "0000010000000000"

         it "test encodeHR  for hr = 256 fm = movement and Yellow signal quality" $ do
           encodeHR 256 Movement SignalYellow `shouldBe` binToWord16 "0010110000000000"

         it "test packHR1 for hr = 256 fm = movement and Yellow signal quality" $ do
           packHR1 (buildHR1 Movement SignalYellow  256 False) [] `shouldBe` [44,0]

         it "test packHR1 for hr = 100 fm = no movement and Red signal quality" $ do
           packHR1 (buildHR1 NoMovement SignalRed 100 False) [] `shouldBe` [1,144]
                          
         it "test encodeHR1 for the correct order of HR values: oldest sample first" $ do
           encodeHR1 [ ( buildHR1 NoMovement SignalRed 100 False)
                      ,( buildHR1 NoMovement SignalRed 101 False)
                      ,( buildHR1 NoMovement SignalRed 102 False)
                      ,( buildHR1 NoMovement SignalRed 103 False)
                     ] `shouldBe` BS.pack [1,156,1,152,1,148,1,144]

         it "test packHR2 for hr = 256 Yellow signal quality" $ do
           packHR2 (buildHR2 SignalYellow 256 False) [] `shouldBe` [36, 0]

         it "test packHR2 for hr = 100 Green signal quality" $ do
           packHR2 (buildHR2 SignalGreen 100 False) [] `shouldBe` [65,144]

         it "test encodeHR2 for the correct order of HR values" $ do
           encodeHR2 [ ( buildHR2 SignalRed 100 False)
                      ,( buildHR2 SignalRed 101 False)
                      ,( buildHR2 SignalRed 102 False)
                      ,( buildHR2 SignalRed 103 False)
                     ] `shouldBe` BS.pack [1,156,1,152,1,148,1,144]

         it "test packMHR for hr = 256 Yellow signal quality" $ do
           packMHR (buildMHR SignalYellow 256 False) [] `shouldBe` [36, 0]

         it "test packMHR for hr = 100 Green signal quality" $ do
           packMHR (buildMHR SignalGreen 100 False) [] `shouldBe` [65,144]

         it "test encodeMHR for the correct order of HR values" $ do
           encodeMHR [ ( buildMHR SignalRed 100 False)
                      ,( buildMHR SignalRed 101 False)
                      ,( buildMHR SignalRed 102 False)
                      ,( buildMHR SignalRed 103 False)
                     ] `shouldBe` BS.pack [1,156,1,152,1,148,1,144]

         it "test packTOCO for toco rate of 100" $ do
             packTOCO (buildTOCO 100) [] `shouldBe` [100]

         it "test encodeTOCO for toco rates in correct order" $ do
           encodeTOCO [ buildTOCO 100
                       ,buildTOCO 101
                       ,buildTOCO 102
                       ,buildTOCO 103
                      ] `shouldBe` BS.pack [103,102,101,100]

         it "test encodeHRModes with HR1 HRMode of NoHRTransducer" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [0,0]

         it "test encodeHRModes with HR1 HRMode of Inop" $ do
           encodeHRModes Inop NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [16,0]

         it "test encodeHRModes with HR1 HRMode of US" $ do
           encodeHRModes US NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [32,0]

         it "test encodeHRModes with HR1 HRMode of DECG" $ do
           encodeHRModes DECG NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [64,0]

         it "test encodeHRModes with HR1 HRMode of Reserved2" $ do
           encodeHRModes Reserved2 NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [192,0]

         it "test encodeHRModes with HR1 HRMode of UnknownHRMode" $ do
           encodeHRModes UnknownHRMode NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [224,0]

         it "test encodeHRModes with HR2 HRMode of NoHRTransducer" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [0,0]

         it "test encodeHRModes with HR2 HRMode of Inop" $ do
           encodeHRModes  NoHRTransducer Inop MHRNoHRTransducer `shouldBe` BS.pack [1,0]

         it "test encodeHRModes with HR2 HRMode of US" $ do
           encodeHRModes NoHRTransducer US MHRNoHRTransducer `shouldBe` BS.pack [2,0]

         it "test encodeHRModes with HR2 HRMode of DECG" $ do
           encodeHRModes NoHRTransducer DECG MHRNoHRTransducer `shouldBe` BS.pack [4,0]

         it "test encodeHRModes with HR2 HRMode of Reserved2" $ do
           encodeHRModes NoHRTransducer Reserved2  MHRNoHRTransducer `shouldBe` BS.pack [12,0]

         it "test encodeHRModes with HR2 HRMode of UnknownHRMode" $ do
           encodeHRModes NoHRTransducer UnknownHRMode MHRNoHRTransducer `shouldBe` BS.pack [14,0]

         it "test encodeHRModes with MHR HRMode of NoHRTransducer" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRNoHRTransducer `shouldBe` BS.pack [0,0]

         it "test encodeHRModes with MHR HRMode of Inop" $ do
           encodeHRModes  NoHRTransducer NoHRTransducer MHRInop `shouldBe` BS.pack [0,16]

         it "test encodeHRModes with MHR HRMode of MECG" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MECG `shouldBe` BS.pack [0,96]

         it "test encodeHRModes with MHR HRMode of ExternalMRH" $ do
           encodeHRModes NoHRTransducer NoHRTransducer ExternalMHR `shouldBe` BS.pack [0,128]

         it "test encodeHRModes with HR2 HRMode of Reserved1" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRReserved1 `shouldBe` BS.pack [0,160]

         it "test encodeHRModes with HR2 HRMode of Reserved2" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRReserved2 `shouldBe` BS.pack [0,192]

         it "test encodeHRModes with HR2 HRMode of UnknownHRMode" $ do
           encodeHRModes NoHRTransducer NoHRTransducer MHRUnknownMode `shouldBe` BS.pack [0,224]

         it "test encodeCTGStatus with MonitorStatus set" $ do
           encodeCTGStatus (setMonitorOn True ctgStatus) 
                            `shouldBe` BS.pack [128,0]

         it "test encodeCTGStatus with Data Inserted status set" $ do
           encodeCTGStatus ( setDataInsertedStatus True ctgStatus)
             `shouldBe` BS.pack [64,0]

         it "test encodeCTGStatus with Data Deleted status set" $ do
           encodeCTGStatus ( setDataDeletedStatus True ctgStatus)
             `shouldBe` BS.pack [32,0]

         it "test encodeCTGStatus with FSPO2 set" $ do
           encodeCTGStatus ( setFSPO2AvailableStatus True ctgStatus)
             `shouldBe` BS.pack [8,0]

         it "test encodeCTGStatus with Telemetry set" $ do
           encodeCTGStatus (setTelemetryOnStatus True ctgStatus)
             `shouldBe` BS.pack [2,0]

         it "test encodeCTGStatus with HRCrossChannelVer set" $ do
           encodeCTGStatus (setHRCrossChannelVer True ctgStatus)
             `shouldBe` BS.pack [1,0]

         it "test encodeCTGStatus with FMP Enabled set" $ do
           encodeCTGStatus (setFMPEnabled True ctgStatus)
             `shouldBe` BS.pack [0,1]

         it "test encodeCTGStatus with DECGLogin set" $ do
           encodeCTGStatus (setDECGLogicOn True ctgStatus)
             `shouldBe` BS.pack [0,32]

         it "test encodeCTGStatus with HR Twin Offset set" $ do
           encodeCTGStatus (setHRTwinOffset True ctgStatus)
             `shouldBe` BS.pack [0,2]
         describe "Test Exposed Methods" $ do
           it "test encodeCTG method" $ do
             (ctgData == ctgParser ( BL.toStrict (encodeCTG ctgData)))  `shouldBe`  True 

         
ctgStatus :: CTGStatus         
ctgStatus = CTGStatus False False False False False False False False False           

ctgStatusAll :: CTGStatus
ctgStatusAll = setHRTwinOffset True
               $ setDECGLogicOn True
               $ setFMPEnabled True
               $ setHRCrossChannelVer True
               $ setTelemetryOnStatus True
               $ setFSPO2AvailableStatus True
               $ setDataDeletedStatus True
               $ setDataInsertedStatus True
               $ setMonitorOn True ctgStatus 
hr1TestRates :: [HR1]
hr1TestRates = [ HR1 Movement $ HR SignalGreen 100 False
               , HR1 Movement $ HR SignalGreen 101 False
               , HR1 Movement $ HR SignalGreen 102 False
               , HR1 Movement $ HR SignalGreen 103 False
               ] 

hr2TestRates :: [HR2]
hr2TestRates = [ HR2 $ HR SignalYellow 200 False
               , HR2 $ HR SignalYellow 201 False
               , HR2 $ HR SignalYellow 202 False
               , HR2 $ HR SignalYellow 203 False
               ]

mhrTestRates :: [MHR]
mhrTestRates = [ MHR $ HR SignalRed 301 False
               , MHR $ HR SignalRed 302 False
               , MHR $ HR SignalRed 303 False
               , MHR $ HR SignalRed 304 False
               ]

tocoTestRates :: [TOCO]
tocoTestRates = [ TOCO 5 ,TOCO 6,TOCO 7,TOCO 8 ]


ctgData :: CTGData
ctgData = CTGData ctgStatusAll hr1TestRates hr2TestRates mhrTestRates tocoTestRates US DECG MECG IUP

-- | convert a decimal value to a string of binary digits such as "1010101"
dec2bin :: Int -> [Char]
dec2bin = reverse . map intToDigit . unfoldr (\x -> if x==0 then Nothing else Just(rem x 2, div x 2))

-- | convert a CTGData instance into a list of strings of binary digits from the bytestring
ctgBinToString:: CTGData -> [String]
ctgBinToString ctgdata = map (cnv2BinStr) (BL.unpack $ encodeCTG ctgdata)
  where cnv2BinStr n = dec2bin $ fromIntegral n

  
