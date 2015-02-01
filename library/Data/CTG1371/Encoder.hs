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
  P.putByteString $ encodeCTGStatus $ ctgStatus ctgData  

encodeCTGStatus :: CTGStatus -> BS.ByteString
encodeCTGStatus  = undefined

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
