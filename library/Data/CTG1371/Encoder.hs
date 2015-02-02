module Data.CTG1371.Encoder where

import Data.CTG1371
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS 
import qualified Data.Binary.Put as P

-- | TODO complete method to encode a CTGData instance into a bytestring 

encodeCTG :: CTGData -> BL.ByteString
encodeCTG ctgData =  P.runPut (buildCTGByteString ctgData)

buildCTGByteString :: CTGData -> P.Put
buildCTGByteString  ctgData = do
  P.putWord8 67
  P.putByteString $ encodeCTGStatus ctgData
  P.putByteString $ encodeHR1 $ getHR1 ctgData
  P.putByteString $ encodeHR2 $ getHR2 ctgData
  P.putByteString $ encodeMHR $ getMHR ctgData
  P.putByteString $ encodeTOCO $ getTOCO ctgData 

encodeCTGStatus :: CTGData -> BS.ByteString
encodeCTGStatus ctgdata  = BS.pack [firstWord,secondWord]
  where firstWord = packWord8 (  getMonitorOnStatus ctgdata
                               , getDataInsertedStatus ctgdata
                               , getDataDeletedStatus ctgdata
                               , False
                               , getFSPO2AvailableStatus ctgdata
                               , False
                               , getTelemetryStatus ctgdata
                               , getHRCrossChannelStatus ctgdata)
        secondWord = packWord8 ( False
                               , False
                               , getDECGLogicStatus ctgdata
                               , False
                               , False
                               , False
                               , getHRTwinOffsetStatus ctgdata
                               , getFMPStatus ctgdata)

packWord8 :: (Bool, Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Word8
packWord8  (a, b, c, d, e, f, g, h)  = z h 1 .|. z g 2 .|. z f 4 .|. z e 8 .|. z d 16 .|. z c 32 .|. z b 64 .|. z a 128
  where z False _ = 0
        z True  n = n

encodeHR1 :: [HR1] -> BS.ByteString
encodeHR1 hr1Values = BS.pack $ foldr packHR1 [] hr1Values

packHR1:: HR1 -> [Word8] -> [Word8]
packHR1 hr1  acc = acc ++  encodeWord16 ( encodeHR
                                             (getHR1HeartRate hr1)
                                             (getHR1FetalMovement hr1)
                                             (getHR1SignalQuality hr1)
                                         ) 
  
encodeHR2 :: [HR2] -> BS.ByteString
encodeHR2 hr2Values = BS.pack $ foldr packHR2 [] hr2Values

packHR2 :: HR2 -> [Word8] -> [Word8]
packHR2 hr2 acc = acc ++ encodeWord16 (encodeHR
                                         (getHR2HeartRate hr2)
                                         NullMovement
                                         (getHR2SignalQuality hr2)
                                      )

encodeMHR :: [MHR] -> BS.ByteString
encodeMHR mhrValues = BS.pack $ foldr packMHR [] mhrValues

packMHR :: MHR -> [Word8] -> [Word8]
packMHR mhr acc = acc ++ encodeWord16 (encodeHR
                                         (getMHRHeartRate mhr)
                                         NullMovement
                                         (getMHRSignalQuality mhr)
                                      )

encodeTOCO :: [TOCO] -> BS.ByteString
encodeTOCO tocovalues= BS.pack $ foldr packTOCO [] tocovalues

packTOCO :: TOCO -> [Word8] -> [Word8]
packTOCO toco acc = acc ++ [getTOCORate toco]


encodeHRMode :: HRMode -> BS.ByteString
encodeHRMode = undefined

encodeTOCOMode :: TOCOMode -> BS.ByteString
encodeTOCOMode = undefined

-- | TODO encode a HR (Heart Rate as a 16 bit word
encodeHR :: Word16 -> FetalMovement -> SignalQuality -> Word16
encodeHR rate movement quality = rate * 4 + calcMovement + calcQuality
  where calcMovement  = if movement  == Movement then 0x800 else 0
        calcQuality = case quality of
                        SignalRed    -> 0
                        SignalYellow -> 0x2000
                        SignalGreen  -> 0x4000
                        

-- | TODO encodes a 16 bit word into two bytes
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = fmap fromIntegral [ (x .&. 0xFF00) `shiftR` 8, x .&. 0xFF ]
