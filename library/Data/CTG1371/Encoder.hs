module Data.CTG1371.Encoder where

import Data.CTG1371
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
--import qualified Data.Binary.Strict.Get as G

-- | TODO complete method to encode a CTGData instance into a bytestring 

encodeCTG :: CTGData -> BS.ByteString
encodeCTG _ =  BS.pack [63,0,0,4,40]

-- | TODO encodes a 16 bit word into two bytes
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = fmap fromIntegral [ (x .&. 0xFF00) `shiftR` 8, x .&. 0xFF ]

-- | TODO encode a HR (Heart Rate_ as a 16 bit word
encodeHR :: Word16 -> Bool -> SignalQuality -> Word16
encodeHR rate movement quality = rate * 4 + calcMovement + calcQuality
  where calcMovement = if movement then 0x800 else 0
        calcQuality = case quality of
                        SignalRed    -> 0
                        SignalYellow -> 0x2000
                        SignalGreen  -> 0x4000
