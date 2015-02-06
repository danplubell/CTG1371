module Data.CTG1371.Internal.Encoder.Encoders where

import Data.CTG1371.Internal.Types
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Binary.Put as P

-- | Encodes a CTGData instance into a Put action
buildCTGByteString :: CTGData -> P.Put
buildCTGByteString  ctgData = do
  P.putWord8 67 -- 'C' indicates a 'C' data block
  P.putByteString $ encodeCTGStatus $ getCTGStatus ctgData
  P.putByteString $ encodeHR1 $ getHR1 ctgData
  P.putByteString $ encodeHR2 $ getHR2 ctgData
  P.putByteString $ encodeMHR $ getMHR ctgData
  P.putByteString $ encodeTOCO $ getTOCO ctgData
  P.putByteString $ encodeHRModes (getHR1Mode ctgData) (getHR2Mode ctgData) (getMHRMode ctgData)
  P.putByteString $ encodeTOCOMode $ getTOCOMode ctgData
  
-- | Encode the CTG status into the ByteString.  The status indicators are stored in two 8 bit words
encodeCTGStatus :: CTGStatus -> BS.ByteString
encodeCTGStatus ctgstatus  = BS.pack [firstWord,secondWord]
  where firstWord = packWord8 (  getMonitorOnStatus ctgstatus
                               , getDataInsertedStatus ctgstatus
                               , getDataDeletedStatus ctgstatus
                               , False
                               , getFSPO2AvailableStatus ctgstatus
                               , False
                               , getTelemetryStatus ctgstatus
                               , getHRCrossChannelStatus ctgstatus)
        secondWord = packWord8 ( False
                               , False
                               , getDECGLogicStatus ctgstatus
                               , False
                               , False
                               , False
                               , getHRTwinOffsetStatus ctgstatus
                               , getFMPStatus ctgstatus)

-- | Pack a string of Bool values into a Word8
packWord8 :: (Bool, Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Word8
packWord8  (a, b, c, d, e, f, g, h)  = z h 1 .|. z g 2 .|. z f 4 .|. z e 8 .|. z d 16 .|. z c 32 .|. z b 64 .|. z a 128
  where z False _ = 0
        z True  n = n

-- | Encode a list of Heart Rate 1 values into a ByteString
encodeHR1 :: [HR1] -> BS.ByteString
encodeHR1 hr1Values = BS.pack $ foldr packHR1 [] hr1Values

-- | Encode a single Heart Rate 1 value and add it to the accumulated ByteString
packHR1:: HR1 -> [Word8] -> [Word8]
packHR1 hr1  acc = acc ++  encodeWord16 ( encodeHR
                                             (getHR1HeartRate hr1)
                                             (getHR1FetalMovement hr1)
                                             (getHR1SignalQuality hr1)
                                         ) 
  
-- | Encode a list of Heart Rate 2 values into a ByteString 
encodeHR2 :: [HR2] -> BS.ByteString
encodeHR2 hr2Values = BS.pack $ foldr packHR2 [] hr2Values

-- | Encodes a single Heart Rate 2 value and adds it to the accumulated ByteString
packHR2 :: HR2 -> [Word8] -> [Word8]
packHR2 hr2 acc = acc ++ encodeWord16 (encodeHR
                                         (getHR2HeartRate hr2)
                                         NullMovement
                                         (getHR2SignalQuality hr2)
                                      )
-- | Encode a list of Maternal Heart Rate values into a ByteString
encodeMHR :: [MHR] -> BS.ByteString
encodeMHR mhrValues = BS.pack $ foldr packMHR [] mhrValues

 -- | Add a encodes Maternal Heart Rate to the accumulated ByteString
packMHR :: MHR -> [Word8] -> [Word8]
packMHR mhr acc = acc ++ encodeWord16 (encodeHR
                                         (getMHRHeartRate mhr)
                                         NullMovement
                                         (getMHRSignalQuality mhr)
                                      )

-- | Encodes the list of TOCO values into a ByteString
encodeTOCO :: [TOCO] -> BS.ByteString
encodeTOCO tocovalues= BS.pack $ foldr packTOCO [] tocovalues

-- | Adds a TOCO rate to the accumulated ByteString
packTOCO :: TOCO -> [Word8] -> [Word8]
packTOCO toco acc = acc ++ [getTOCORate toco]


-- | Encodes the heart rate modes.  The heart modes are stored in two bytes
encodeHRModes :: HRMode->HRMode->HRMode -> BS.ByteString
encodeHRModes hr1Mode hr2Mode mhrMode = BS.pack $ encodeWord16 (translateHRMode hr1Mode `shiftL` 12
                                                               .|. translateHRMode hr2Mode `shiftL` 8
                                                               .|. translateHRMode mhrMode `shiftL` 4)
                                                               
  where translateHRMode hrMode= case hrMode of
          NoHRTransducer -> 0
          Inop           -> 1
          US             -> 2
          DECG           -> 4
          MECG           -> 6
          ExternalMRH    -> 8
          Reserved1      -> 10
          Reserved2      -> 12
          UnknownHRMode  -> 14
          NullHRMode     -> 0 
-- | Encode the toco mode into a ByteString                                  
encodeTOCOMode :: TOCOMode -> BS.ByteString
encodeTOCOMode tocoMode = BS.pack  [translateTOCOMode]
  where translateTOCOMode = case tocoMode of
          NoTOCOTransducer -> 0
          ExternalTOCO     -> 8
          IUP              -> 10
          UnknownTOCOMode  -> 14
          NullTOCOMode     -> 0 

-- | Encode a HR (Heart Rate as a 16 bit word)
encodeHR :: Word16 -> FetalMovement -> SignalQuality -> Word16
encodeHR rate movement quality = rate * 4 + calcMovement + calcQuality
  where calcMovement  = if movement  == Movement then 0x800 else 0
        calcQuality = case quality of
                        SignalRed    -> 0
                        SignalYellow -> 0x2000
                        SignalGreen  -> 0x4000
                        

-- | Encodes a 16 bit word into two bytes
encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = fmap fromIntegral [ (x .&. 0xFF00) `shiftR` 8, x .&. 0xFF ]
