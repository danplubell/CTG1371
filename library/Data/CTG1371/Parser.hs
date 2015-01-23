module Data.CTG1371.Parser (ctgParser
                           , CTGStatus(..)
                           , CTGData(..)
                           , SignalQuality(..)
                           , FetalMovement(..)
                           , HR(..)
                           , HR1(..)
                           , HR2(..)
                           , MHR(..)
                           , TOCO(..)
                           , HRMode(..)
                           , TOCOMode(..))  where

import Data.Word
import Data.Bits
import Data.ByteString
import qualified Data.Binary.Strict.Get as G

-- | TODO
data CTGStatus = CTGStatus { fmpEnabled::Bool        
                           , hrTwinOffsetOn::Bool     
                           , decgLogicOn::Bool
                           , hrCrossChannelVer::Bool
                           , telemetryOn::Bool
                           , fspo2Available::Bool
                           , ctgDataDeleted::Bool
                           , ctgDataInserted::Bool
                           , monitorOn::Bool
                           } deriving (Show)
                                      
data SignalQuality = SignalRed | SignalYellow | SignalGreen deriving (Show)

data FetalMovement = Movement | NoMovement deriving (Show)

data HR = HR { signalQuality::SignalQuality
             , heartRate::Int
             } | BlankTrace  deriving (Show)

data HR1 = HR1 { fmp::FetalMovement
               , hr1::HR  
               } deriving (Show) 

data HR2 = HR2 { hr2::HR } deriving (Show)

data MHR = MHR { mhr::HR } deriving (Show)

data TOCO = TOCO { tocoRate::Int } deriving (Show)

data HRMode = Inop | NoHRTransducer | US | DECG | MECG | ExternalMRH | UnknownHRMode |Reserved deriving (Show)

data TOCOMode = NoTOCOTransducer | ExternalTOCO | IUP | UnknownTOCOMode deriving (Show)

data CTGData = CTGData { ctgStatus::CTGStatus
                       , ctgHR1::[HR1]
                       , ctgHR2::[HR2]
                       , ctgMHR::[MHR]
                       , ctgToco::[TOCO]
                       , ctgHR1Mode::HRMode
                       , ctgHR2Mode::HRMode
                       , ctgMHRMode::HRMode
                       , ctgTocoMode::TOCOMode
                       } deriving (Show)
--testData::ByteString                                  
--testData = pack [66, -128, 32, 65, 104, 65, 104, 65, 104, 65, 104, 66, 40, 66, 40, 66, 40, 66, 40, 65, -32, 65, -32, 65, -32, 65, -32, 4, 4, 4, 4, 34, 96, 10, 0]
--testData = pack [67, -128, 0, 33, 91, 33, 91, 33, 91, 33, 91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34, 2, 0, 0]
--testParse::IO()
--testParse = print $ ctgParser testData

--expose this module
ctgParser::ByteString -> CTGData
ctgParser ctgdata =do
  let (parsedData,_)  = G.runGet parseCTG ctgdata
  case parsedData of
    Left errMsg -> error $ "ctgParseError: " ++ errMsg
    Right x -> x

--function used by runget to parse the data
parseCTG ::  G.Get CTGData
parseCTG = do
  c         <- G.getWord8
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

{-
dumpCTG::G.Get (Int,Int,Int,Int,Int,Int,Int)
dumpCTG = do
  _ <- G.getWord8
  status <- fmap fromIntegral G.getWord16be
  hr1Block4 <- fmap fromIntegral G.getWord16be 
  hr1Block3 <- fmap fromIntegral G.getWord16be
  hr1Block2 <- fmap fromIntegral G.getWord16be
  hr1Block1 <- fmap fromIntegral G.getWord16be
  hr2Block4 <- fmap fromIntegral G.getWord16be
  hr2Block3 <- fmap fromIntegral G.getWord16be
  hr2Block2 <- fmap fromIntegral G.getWord16be
  hr2Block1 <- fmap fromIntegral G.getWord16be

  mhrBlock4 <- fmap fromIntegral G.getWord16be
  mhrBlock3 <- fmap fromIntegral G.getWord16be
  mhrBlock2 <- fmap fromIntegral G.getWord16be
  mhrBlock1 <- fmap fromIntegral G.getWord16be

  tocoBlock4 <- fmap fromIntegral G.getWord8
  tocoBlock3 <- fmap fromIntegral G.getWord8
  tocoBlock2 <- fmap fromIntegral G.getWord8
  tocoBlock1 <- fmap fromIntegral G.getWord8

  hrMode <- fmap fromIntegral G.getWord16be
  tocoMode <- fmap fromIntegral G.getWord8
  
  return (status,hr1Block4,hr1Block3,hr1Block2,hr1Block1,hr2Block4,hrMode)

runDump =  print $ G.runGet dumpCTG testData
-}             
--unpack an hr block
unpackHR1:: Word16 -> HR1
unpackHR1 hrdata = HR1 checkFetalMovement (unpackHR hrdata)
  where checkFetalMovement = if testBit hrdata 12 then Movement else NoMovement

unpackHRMode :: Word16 -> (HRMode,HRMode,HRMode)
unpackHRMode hrdata = (unpackHR1Mode hrdata,unpackHR2Mode hrdata,unpackMHRMode hrdata)
                       
unpackHR1Mode :: Word16 -> HRMode
unpackHR1Mode hrdata = translateHRMode $ (hrdata .&. 0xF000) `shiftR` 12 

unpackHR2Mode::Word16 -> HRMode
unpackHR2Mode hrdata = translateHRMode $ (hrdata .&. 0xF00) `shiftR` 8

unpackMHRMode::Word16 -> HRMode
unpackMHRMode hrdata = translateHRMode (hrdata .&. 0xF0)

unpackTocoMode :: Word8 -> TOCOMode
unpackTocoMode tocodata = case tocodata of
                           0  -> NoTOCOTransducer
                           8  -> ExternalTOCO
                           10 -> IUP
                           14 -> UnknownTOCOMode
                           _ -> UnknownTOCOMode

translateHRMode :: (Num a, Eq a) => a -> HRMode
translateHRMode hrdata = case hrdata of
                           0  -> NoHRTransducer
                           1  -> Inop
                           2  -> US
                           4  -> DECG
                           6  -> MECG
                           8  -> ExternalMRH
                           10 -> Reserved
                           12 -> Reserved
                           14 -> UnknownHRMode
                           _  -> UnknownHRMode
unpackToco :: Word8 -> TOCO
unpackToco tocodata = TOCO (fromIntegral tocodata)

unpackHR::Word16 -> HR
unpackHR hrdata = HR getSignalStrength getHR 
  where getSignalStrength = case hrdata `shiftR` 13 of
                              0 -> SignalRed
                              1 -> SignalYellow
                              2 -> SignalGreen
                              _ -> error "Invalid signal strength value"
        getHR = fromIntegral  ((hrdata `shiftL` 5) `shiftR` 5) `div` 4

unpackHR2 :: Word16 -> HR2
unpackHR2 hrdata = HR2 (unpackHR hrdata)

unpackMHR ::Word16 -> MHR
unpackMHR hrdata = MHR (unpackHR hrdata)

parseStatus :: Word16 -> CTGStatus 
parseStatus status =  CTGStatus
                        isFMPEnabled
                        ishrTwinOffsetOn
                        isdecgLogicOn
                        ishrCrossChanVerOn
                        isTelemetryOn
                        isfspo2Available
                        isctgDataDeleted
                        isctgDataInserted
                        isMonitorOn
  where isFMPEnabled       = testBit status 0
        ishrTwinOffsetOn   = testBit status 1
        isdecgLogicOn      = testBit status 5
        ishrCrossChanVerOn = testBit status 8
        isTelemetryOn      = testBit status 9
        isfspo2Available   = testBit status 11
        isctgDataDeleted   = testBit status 13
        isctgDataInserted  = testBit status 14
        isMonitorOn        = testBit status 15
