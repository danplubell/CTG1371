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
buildCTGByteString  ctgData = 
  P.putByteString $ encodeCTGStatus ctgData  

encodeCTGStatus :: CTGData -> BS.ByteString
encodeCTGStatus ctgdata  = BS.pack [firstWord,secondWord]
  where firstWord = packWord8 ( getMonitorOnStatus ctgdata
                               , getDataInsertedStatus ctgdata
                               , getDataDeletedStatus ctgdata
                               , False
                               , getFSPO2AvailableStatus ctgdata
                               , False
                               , getTelemetryStatus ctgdata
                               , getHRCrossChannelStatus ctgdata)
        secondWord = packWord8 (True,True,True,True,True,True,True,True)

packWord8 :: (Bool, Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Word8
packWord8  (a, b, c, d, e, f, g, h) = z h 1 .|. z g 2 .|. z f 4 .|. z e 8 .|. z d 16 .|. z c 32 .|. z b 64 .|. z a 128
  where z False _ = 0
        z True  n = n

encodeHR1 :: [HR1] -> BS.ByteString
encodeHR1 = undefined

encodeHR2 :: [HR2] -> BS.ByteString
encodeHR2 = undefined

encodeMHR :: MHR -> BS.ByteString
encodeMHR = undefined

encodeTOCO :: TOCO -> BS.ByteString
encodeTOCO = undefined

encodeHRMode :: HRMode -> BS.ByteString
encodeHRMode = undefined

encodeTOCOMode :: TOCOMode -> BS.ByteString
encodeTOCOMode = undefined

-- | TODO encode a HR (Heart Rate as a 16 bit word
encodeHR :: Word16 -> Bool -> SignalQuality -> Word16
encodeHR rate movement quality = rate * 4 + calcMovement + calcQuality
  where calcMovement = if movement then 0x800 else 0
        calcQuality = case quality of
                        SignalRed    -> 0
                        SignalYellow -> 0x2000
                        SignalGreen  -> 0x4000
                        

-- | TODO encodes a 16 bit word into two bytes
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = fmap fromIntegral [ (x .&. 0xFF00) `shiftR` 8, x .&. 0xFF ]
