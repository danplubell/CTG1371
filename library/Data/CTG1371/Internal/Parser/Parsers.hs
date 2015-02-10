module Data.CTG1371.Internal.Parser.Parsers where

import Data.CTG1371.Internal.Types
import Data.Word
import Data.Bits
import qualified Data.Binary.Strict.Get as G

{-|This function does all the parsing  -}
parseCTG ::  G.Get CTGData
parseCTG = do
  c         <- G.getWord8 --Is it a "C" block
  if c /= 67 then fail "Submitted data is not a C block" else do  
    status    <- fmap parseStatus G.getWord16be

    hr1Block4 <- fmap unpackHR1 G.getWord16be
    hr1Block3 <- fmap unpackHR1 G.getWord16be
    hr1Block2 <- fmap unpackHR1 G.getWord16be
    hr1Block1 <- fmap unpackHR1 G.getWord16be

    hr2Block4 <- fmap unpackHR2 G.getWord16be 
    hr2Block3 <- fmap unpackHR2 G.getWord16be
    hr2Block2 <- fmap unpackHR2 G.getWord16be
    hr2Block1 <- fmap unpackHR2 G.getWord16be

    mhrBlock4 <- fmap unpackMHR G.getWord16be
    mhrBlock3 <- fmap unpackMHR G.getWord16be
    mhrBlock2 <- fmap unpackMHR G.getWord16be
    mhrBlock1 <- fmap unpackMHR G.getWord16be

    tocoBlock4 <- fmap unpackToco G.getWord8
    tocoBlock3 <- fmap unpackToco G.getWord8
    tocoBlock2 <- fmap unpackToco G.getWord8
    tocoBlock1 <- fmap unpackToco G.getWord8

    (hr1Mode,hr2Mode,mhrMode)     <- fmap unpackHRMode G.getWord16be 
    tocoMode   <- fmap unpackTocoMode G.getWord8
  
    return (CTGData
            status
            [hr1Block1,hr1Block2,hr1Block3,hr1Block4]
            [hr2Block1,hr2Block2,hr2Block3,hr2Block4]
            [mhrBlock1,mhrBlock2,mhrBlock3,mhrBlock4]
            [tocoBlock1,tocoBlock2,tocoBlock3,tocoBlock4]
            hr1Mode hr2Mode mhrMode tocoMode)

-- | Unpack the first heart rate value 
unpackHR1:: Word16 -> HR1
unpackHR1 hrdata = HR1 checkFetalMovement (unpackHR hrdata)
  where checkFetalMovement = if testBit hrdata 11  then Movement else NoMovement

-- | Unpack the heart rate mode values
unpackHRMode :: Word16 -> (HRMode,HRMode,MHRMode)
unpackHRMode hrdata = (unpackHR1Mode hrdata,unpackHR2Mode hrdata,unpackMHRMode hrdata)

-- | Unpack the first heart rate mode 
unpackHR1Mode :: Word16 -> HRMode
unpackHR1Mode hrdata = translateHRMode $ (hrdata .&. 0xF000) `shiftR` 12 

-- | Unpack the second heart rate mode
unpackHR2Mode::Word16 -> HRMode
unpackHR2Mode hrdata = translateHRMode $ (hrdata .&. 0xF00) `shiftR` 8

-- | Unpack the maternal heart rate mode
unpackMHRMode::Word16 -> MHRMode
unpackMHRMode hrdata = translateMHRMode $  (hrdata .&. 0xF0) `shiftR` 4


-- | Unpack the tocography mode
unpackTocoMode :: Word8 -> TOCOMode
unpackTocoMode tocodata = case tocodata of
                           0  -> NoTOCOTransducer
                           8  -> ExternalTOCO
                           10 -> IUP
                           14 -> UnknownTOCOMode
                           _  -> NullTOCOMode 

-- | translate a numeric heart rate mode into the symbolic heart rate mode
translateHRMode :: (Num a, Eq a) => a -> HRMode
translateHRMode hrdata = case hrdata of
                           0  -> NoHRTransducer
                           1  -> Inop
                           2  -> US
                           4  -> DECG
                           12 -> Reserved2
                           14 -> UnknownHRMode
                           _  -> NullHRMode
-- | translate a numeric maternal heart rate into the symbolic heart rate mode
translateMHRMode :: (Num a, Eq a) => a -> MHRMode
translateMHRMode mhrdata = case mhrdata of
  0  -> MHRNoHRTransducer
  1  -> MHRInop
  6  -> MECG
  8  -> ExternalMHR
  10 -> MHRReserved1
  12 -> MHRReserved2
  _  -> MHRNullHRMode

-- | Unpack a tocography value
unpackToco :: Word8 -> TOCO
unpackToco tocodata = TOCO (fromIntegral tocodata)

-- | Unpack a first heart rate value
unpackHR::Word16 -> HR
unpackHR hrdata = HR getSignalQualityInd getHR isBlankTrace 
  where getSignalQualityInd = case hrdata `shiftR` 13 of
                              0 -> SignalRed
                              1 -> SignalYellow
                              2 -> SignalGreen
                              _ -> error "Invalid signal quality value"
        getHR = fromIntegral  ((hrdata `shiftL` 5) `shiftR` 5) `div` 4
        isBlankTrace = getHR == 0 
        
-- | Unpack a second heart rate value
unpackHR2 :: Word16 -> HR2
unpackHR2 hrdata = HR2 (unpackHR hrdata)

-- | Unpack the maternal heart rate
unpackMHR ::Word16 -> MHR
unpackMHR hrdata = MHR (unpackHR hrdata)

-- | Parse the status into symbolic values
parseStatus :: Word16 -> CTGStatus 
parseStatus status =  CTGStatus
                        isMonitorOn
                        isctgDataInserted
                        isctgDataDeleted
                        isfspo2Available
                        isTelemetryOn
                        ishrCrossChanVerOn
                        isdecgLogicOn
                        ishrTwinOffsetOn
                        isFMPEnabled
  where isFMPEnabled       = testBit status 0
        ishrTwinOffsetOn   = testBit status 1
        isdecgLogicOn      = testBit status 5
        ishrCrossChanVerOn = testBit status 8
        isTelemetryOn      = testBit status 9
        isfspo2Available   = testBit status 11
        isctgDataDeleted   = testBit status 13
        isctgDataInserted  = testBit status 14
        isMonitorOn        = testBit status 15

