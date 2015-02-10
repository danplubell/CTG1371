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
                     , setMonitorOn
                     , setDataInsertedStatus
                     , setDataDeletedStatus
                     , setFSPO2AvailableStatus
                     , setTelemetryOnStatus
                     , setHRCrossChannelVer
                     , setFMPEnabled
                     , setDECGLogicOn
                     , setHRTwinOffset
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
                     , MHRMode (..)
                     , TOCOMode(..)) where
import Prelude hiding (id, (.), mod)
import Data.Label
import Data.Word
import Control.Category

-- | Values from the status block of CTG data
data CTGStatus = CTGStatus { _monitorOn::Bool
                           , _ctgDataInserted::Bool        
                           , _ctgDataDeleted::Bool
                           , _fspo2Available::Bool
                           , _telemetryOn::Bool
                           , _hrCrossChannelVer::Bool
                           , _decgLogicOn::Bool
                           , _hrTwinOffsetOn::Bool     
                           , _fmpEnabled::Bool
                           } deriving (Show,Eq)

mkLabels [''CTGStatus]

setMonitorOn :: Bool -> CTGStatus -> CTGStatus
setMonitorOn = set monitorOn

setDataInsertedStatus :: Bool -> CTGStatus -> CTGStatus
setDataInsertedStatus = set ctgDataInserted

setDataDeletedStatus :: Bool -> CTGStatus -> CTGStatus
setDataDeletedStatus = set ctgDataDeleted

setFSPO2AvailableStatus :: Bool -> CTGStatus -> CTGStatus
setFSPO2AvailableStatus = set fspo2Available

setTelemetryOnStatus :: Bool -> CTGStatus -> CTGStatus
setTelemetryOnStatus = set telemetryOn

setHRCrossChannelVer :: Bool -> CTGStatus -> CTGStatus
setHRCrossChannelVer = set hrCrossChannelVer

setFMPEnabled :: Bool -> CTGStatus -> CTGStatus
setFMPEnabled = set fmpEnabled

setDECGLogicOn :: Bool -> CTGStatus -> CTGStatus
setDECGLogicOn = set decgLogicOn

setHRTwinOffset :: Bool -> CTGStatus -> CTGStatus
setHRTwinOffset = set hrTwinOffsetOn

-- | Represents the symbolic value of the signal quality
data SignalQuality = SignalRed | SignalYellow | SignalGreen deriving (Show,Eq)

-- | Represents the symbolic value of the fetal movement indicator
data FetalMovement = Movement | NoMovement | NullMovement deriving (Show,Eq)

-- | Represents a single heart rate value
data HR = HR { _signalQuality::SignalQuality
             , _heartRate::Word16
             , _blankTrace::Bool
             }  deriving (Show,Eq)

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
               } deriving (Show,Eq) 
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
data HR2 = HR2 { _hr2::HR } deriving (Show,Eq)
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
data MHR = MHR { _mhr::HR } deriving (Show,Eq)

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
data TOCO = TOCO { _tocoRate::Word8 } deriving (Show,Eq)
mkLabels [''TOCO]

-- | Gets the tocography rate from a tocography value
getTOCORate :: TOCO -> Word8
getTOCORate  = get tocoRate 

buildTOCO :: Word8 -> TOCO
buildTOCO  = TOCO 

-- | Represents the possible heart rate modes
data HRMode = Inop | NoHRTransducer | US | DECG | UnknownHRMode | Reserved2 | NullHRMode  deriving (Show,Eq)

-- | Represents the possible maternal heart rate modes
data MHRMode = MHRInop | MHRNoHRTransducer | MECG | ExternalMHR | MHRReserved1 | MHRReserved2 | MHRUnknownMode | MHRNullHRMode deriving (Show,Eq)

-- | Represents the possible tocography modes
data TOCOMode = NoTOCOTransducer | ExternalTOCO | IUP | UnknownTOCOMode | NullTOCOMode  deriving (Show,Eq)

data CTGData = CTGData { _ctgStatus::CTGStatus
                       , _ctgHR1::[HR1]
                       , _ctgHR2::[HR2]
                       , _ctgMHR::[MHR]
                       , _ctgToco::[TOCO]
                       , _ctgHR1Mode::HRMode
                       , _ctgHR2Mode::HRMode
                       , _ctgMHRMode::MHRMode
                       , _ctgTocoMode::TOCOMode
                       }  deriving (Show,Eq)

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
getMHRMode :: CTGData -> MHRMode
getMHRMode = get ctgMHRMode

-- | Get the mode of the tocography data
getTOCOMode :: CTGData -> TOCOMode
getTOCOMode = get ctgTocoMode

-- | Get the FMP status
getFMPStatus :: CTGStatus -> Bool
getFMPStatus  = get fmpEnabled 

-- | Get the heart rate twin offset status
getHRTwinOffsetStatus :: CTGStatus -> Bool
getHRTwinOffsetStatus  = get hrTwinOffsetOn

-- | Get the DECG logic status
getDECGLogicStatus::CTGStatus -> Bool
getDECGLogicStatus = get decgLogicOn

-- | Get the heart rate cross channel verification status
getHRCrossChannelStatus :: CTGStatus -> Bool
getHRCrossChannelStatus = get hrCrossChannelVer

-- | Get the telemetry status
getTelemetryStatus :: CTGStatus -> Bool
getTelemetryStatus = get telemetryOn

-- | Get the FSPO2 status 
getFSPO2AvailableStatus :: CTGStatus -> Bool
getFSPO2AvailableStatus = get fspo2Available

-- | Get the data deleted status
getDataDeletedStatus :: CTGStatus -> Bool
getDataDeletedStatus = get ctgDataDeleted

-- | Get the data inserted status
getDataInsertedStatus :: CTGStatus -> Bool
getDataInsertedStatus = get ctgDataInserted

-- | Get the monitor on status
getMonitorOnStatus :: CTGStatus -> Bool
getMonitorOnStatus = get monitorOn 

