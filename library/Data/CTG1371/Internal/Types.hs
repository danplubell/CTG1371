{-# LANGUAGE TemplateHaskell #-}
module Data.CTG1371.Internal.Types (
                       getFMPStatus
                     , getHRTwinOffsetStatus
                     , getDECGLogicStatus
                     , getHRCrossChannelStatus
                     , getTelemetryStatus
                     , getFSPO2AvailableStatus
                     , getDataDeletedStatus
                     , getDataInsertedStatus
                     , getMonitorOnStatus
                     , getHR1
                     , getHR2
                     , getMHR
                     , getTOCO
                     , getHR2Mode
                     , getHR1Mode
                     , getMHRMode
                     , getTOCOMode
                     , getSignalQuality
                     , setSignalQuality
                     , getHeartRate
                     , setHeartRate
                     , getBlankTraceInd
                     , setBlankTraceInd
                     , getHR1SignalQuality
                     , setHR1SignalQuality
                     , getHR1HeartRate
                     , setHR1HeartRate
                     , getHR1FetalMovement
                     , setHR1FetalMovement
                     , buildHR1
                     , getHR2SignalQuality
                     , getHR2HeartRate
                     , buildHR2 
                     , getMHRSignalQuality
                     , getMHRHeartRate
                     , buildMHR
                     , getTOCORate
                     , getCTGStatus
                     , buildTOCO
                     , CTGData(..)
                     , CTGStatus(..)
                     , SignalQuality(..)
                     , FetalMovement(..)
                     , HR(..)
                     , HR1(..)
                     , HR2(..)
                     , MHR(..)
                     , TOCO(..)
                     , HRMode(..)
                     , TOCOMode(..)) where
import Prelude hiding (id, (.), mod)
import Data.Label
import Data.Word
import Control.Category


-- | Values from the status block of CTG data
data CTGStatus = CTGStatus { _fmpEnabled::Bool        
                           , _hrTwinOffsetOn::Bool     
                           , _decgLogicOn::Bool
                           , _hrCrossChannelVer::Bool
                           , _telemetryOn::Bool
                           , _fspo2Available::Bool
                           , _ctgDataDeleted::Bool
                           , _ctgDataInserted::Bool
                           , _monitorOn::Bool
                           } deriving (Show)

mkLabels [''CTGStatus]

-- | Represents the symbolic value of the signal quality
data SignalQuality = SignalRed | SignalYellow | SignalGreen deriving (Show)

-- | Represents the symbolic value of the fetal movement indicator
data FetalMovement = Movement | NoMovement | NullMovement deriving (Show,Eq)

-- | Represents a single heart rate value
data HR = HR { _signalQuality::SignalQuality
             , _heartRate::Word16
             , _blankTrace::Bool
             }  deriving (Show)

mkLabels [''HR]

-- | Get the Signal Quality from a heart rate value
getSignalQuality :: HR -> SignalQuality
getSignalQuality = get signalQuality

-- | Set the Signal Quality for a heart rate value
setSignalQuality :: SignalQuality->HR->HR
setSignalQuality = set signalQuality

-- | Get the heart rate value
getHeartRate :: HR -> Word16
getHeartRate = get heartRate

-- | Set the heart rate value
setHeartRate :: Word16 -> HR -> HR
setHeartRate = set heartRate

-- | Get the blank trace indicator
getBlankTraceInd :: HR -> Bool
getBlankTraceInd = get blankTrace

-- | Set the blank trace indicator
setBlankTraceInd :: Bool -> HR -> HR
setBlankTraceInd = set blankTrace

-- | Represents the first heart rate value
data HR1 = HR1 { _fmp::FetalMovement
               , _hr1::HR  
               } deriving (Show) 
mkLabels [''HR1]

-- | Gets the fetal movement value from a first heart rate value
getHR1FetalMovement :: HR1 -> FetalMovement
getHR1FetalMovement = get fmp

-- | Set the fetal movement value for the first heart rate value
setHR1FetalMovement :: FetalMovement -> HR1 -> HR1
setHR1FetalMovement = set fmp

-- | Gets the signal quality value from a first heart rate value
getHR1SignalQuality :: HR1 -> SignalQuality
getHR1SignalQuality hr1data = get signalQuality $ get hr1 hr1data 

-- | set the signal quality value for the first heart rate value
setHR1SignalQuality :: SignalQuality->HR1->HR1
setHR1SignalQuality = set (signalQuality.hr1)

-- | Gets the heart value from a first heart rate value
getHR1HeartRate :: HR1 -> Word16
getHR1HeartRate hr1data = get heartRate $ get hr1 hr1data
-- | Set the heart rate for the first heart rate value

setHR1HeartRate :: Word16 -> HR1 -> HR1
setHR1HeartRate = set (heartRate.hr1)

-- | Build an HR1 value
buildHR1 :: FetalMovement -> SignalQuality -> Word16 -> Bool -> HR1
buildHR1 fm sq hr bt = HR1 fm (HR sq hr bt)
-- | Represents a second heart rate
data HR2 = HR2 { _hr2::HR } deriving (Show)
mkLabels [''HR2]

-- | Get the signal quality from a second heart rate value
getHR2SignalQuality :: HR2 -> SignalQuality
getHR2SignalQuality hr2data = get signalQuality $ get hr2 hr2data

-- | Get the heart rate from a second heart rate value
getHR2HeartRate :: HR2 -> Word16
getHR2HeartRate hr2data = get heartRate $ get hr2 hr2data

buildHR2 :: SignalQuality->Word16 -> Bool -> HR2
buildHR2 sq hr bt = HR2 (HR sq hr bt)

-- | Represents the maternal heart rate
data MHR = MHR { _mhr::HR } deriving (Show)

mkLabels [''MHR]

-- | Get the signal quality from a maternal heart rate value
getMHRSignalQuality :: MHR -> SignalQuality
getMHRSignalQuality mhrdata = get signalQuality $ get mhr mhrdata

-- | Get the heart rate value from a maternal heart rate value
getMHRHeartRate :: MHR -> Word16
getMHRHeartRate mhrdata = get heartRate $ get mhr mhrdata

buildMHR :: SignalQuality -> Word16 -> Bool ->  MHR
buildMHR sq hr bt = MHR(HR sq hr bt)

-- | Represents a tocography value
data TOCO = TOCO { _tocoRate::Word8 } deriving (Show)
mkLabels [''TOCO]

-- | Gets the tocography rate from a tocography value
getTOCORate :: TOCO -> Word8
getTOCORate  = get tocoRate 

buildTOCO :: Word8 -> TOCO
buildTOCO tr = TOCO tr

-- | Represents the possible heart rate modes
data HRMode = Inop | NoHRTransducer | US | DECG | MECG | ExternalMRH | UnknownHRMode |Reserved1 | Reserved2 | NullHRMode  deriving (Show)

-- | Represents the possible tocography modes
data TOCOMode = NoTOCOTransducer | ExternalTOCO | IUP | UnknownTOCOMode | NullTOCOMode  deriving (Show)

data CTGData = CTGData { _ctgStatus::CTGStatus
                       , _ctgHR1::[HR1]
                       , _ctgHR2::[HR2]
                       , _ctgMHR::[MHR]
                       , _ctgToco::[TOCO]
                       , _ctgHR1Mode::HRMode
                       , _ctgHR2Mode::HRMode
                       , _ctgMHRMode::HRMode
                       , _ctgTocoMode::TOCOMode
                       }  deriving (Show)

mkLabels [ ''CTGData]

-- | Get the status information for an instance of cardiotocography data
getCTGStatus :: CTGData -> CTGStatus
getCTGStatus  = get ctgStatus 

-- | Get the set of first heart rate values from an instance of cardiotocography data
getHR1 :: CTGData -> [HR1]
getHR1 = get ctgHR1

-- | Get the set of second heart rate values from an instance of cardiotocography data
getHR2 :: CTGData -> [HR2]
getHR2 = get ctgHR2

-- | Get the set of maternal heart values from an instance of cardiotocography data
getMHR :: CTGData -> [MHR]
getMHR = get ctgMHR

-- | Get the set of tocography values from an instance of cardiotocography data
getTOCO :: CTGData -> [TOCO]
getTOCO = get ctgToco

-- | Get the heart rate mode of the first heart rate mode
getHR1Mode :: CTGData -> HRMode
getHR1Mode = get ctgHR1Mode

-- | Get the heart rate mode of the second heart rate mode
getHR2Mode :: CTGData -> HRMode
getHR2Mode = get ctgHR2Mode

-- | Get the maternal rate mode
getMHRMode :: CTGData -> HRMode
getMHRMode = get ctgMHRMode

-- | Get the mode of the tocography data
getTOCOMode :: CTGData -> TOCOMode
getTOCOMode = get ctgTocoMode

-- | Get the FMP status
getFMPStatus :: CTGData -> Bool
getFMPStatus  ctgData = get fmpEnabled $  getCTGStatus ctgData

-- | Get the heart rate twin offset status
getHRTwinOffsetStatus :: CTGData -> Bool
getHRTwinOffsetStatus  ctgData = get hrTwinOffsetOn $ getCTGStatus ctgData

-- | Get the DECG logic status
getDECGLogicStatus::CTGData -> Bool
getDECGLogicStatus ctgData  = get decgLogicOn $  getCTGStatus ctgData

-- | Get the heart rate cross channel verification status
getHRCrossChannelStatus :: CTGData -> Bool
getHRCrossChannelStatus ctgData  = get hrCrossChannelVer $  getCTGStatus ctgData

-- | Get the telemetry status
getTelemetryStatus :: CTGData -> Bool
getTelemetryStatus ctgData  = get telemetryOn $  getCTGStatus ctgData

-- | Get the FSPO2 status 
getFSPO2AvailableStatus :: CTGData -> Bool
getFSPO2AvailableStatus ctgData  = get fspo2Available $  getCTGStatus ctgData

-- | Get the data deleted status
getDataDeletedStatus :: CTGData -> Bool
getDataDeletedStatus ctgData  = get ctgDataDeleted $  getCTGStatus ctgData

-- | Get the data inserted status
getDataInsertedStatus :: CTGData -> Bool
getDataInsertedStatus ctgData = get ctgDataInserted $  getCTGStatus ctgData

-- | Get the monitor on status
getMonitorOnStatus :: CTGData -> Bool
getMonitorOnStatus ctgData  = get monitorOn $  getCTGStatus ctgData

