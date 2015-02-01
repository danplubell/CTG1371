{-# LANGUAGE TemplateHaskell #-}
module Data.CTG1371 (getFMPStatus
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
                     , getHeartRate
                     , getHR1SignalQuality
                     , getHR1HeartRate
                     , getHR1FetalMovement
                     , getHR2SignalQuality
                     , getHR2HeartRate
                     , getMHRSignalQuality
                     , getMHRHeartRate
                     , getTOCORate
                     , CTGData(..)
                     , SignalQuality(..)
                     , FetalMovement(..)
                     , HR(..)
                     , HR1(..)
                     , HR2(..)
                     , MHR(..)
                     , TOCO(..)
                     , HRMode(..)
                     , TOCOMode(..)) where
import Data.Label

-- | TODO
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

data SignalQuality = SignalRed | SignalYellow | SignalGreen | BlankTrace deriving (Show)

data FetalMovement = Movement | NoMovement deriving (Show)

data HR = HR { _signalQuality::SignalQuality
             , _heartRate::Int
             }   deriving (Show)

mkLabels [''HR]

getSignalQuality :: HR -> SignalQuality
getSignalQuality = get signalQuality

getHeartRate :: HR -> Int
getHeartRate = get heartRate


data HR1 = HR1 { _fmp::FetalMovement
               , _hr1::HR  
               } deriving (Show) 
mkLabels [''HR1]

getHR1FetalMovement :: HR1 -> FetalMovement
getHR1FetalMovement = get fmp

getHR1SignalQuality :: HR1 -> SignalQuality
getHR1SignalQuality hr1data = get signalQuality $ get hr1 hr1data 

getHR1HeartRate :: HR1 -> Int
getHR1HeartRate hr1data = get heartRate $ get hr1 hr1data


data HR2 = HR2 { _hr2::HR } deriving (Show)
mkLabels [''HR2]

getHR2SignalQuality :: HR2 -> SignalQuality
getHR2SignalQuality hr2data = get signalQuality $ get hr2 hr2data

getHR2HeartRate :: HR2 -> Int
getHR2HeartRate hr2data = get heartRate $ get hr2 hr2data

data MHR = MHR { _mhr::HR } deriving (Show)

mkLabels [''MHR]

getMHRSignalQuality :: MHR -> SignalQuality
getMHRSignalQuality mhrdata = get signalQuality $ get mhr mhrdata

getMHRHeartRate :: MHR -> Int
getMHRHeartRate mhrdata = get heartRate $ get mhr mhrdata

data TOCO = TOCO { _tocoRate::Int } deriving (Show)
mkLabels [''TOCO]

getTOCORate :: TOCO -> Int
getTOCORate  = get tocoRate 

data HRMode = Inop | NoHRTransducer | US | DECG | MECG | ExternalMRH | UnknownHRMode |Reserved deriving (Show)

data TOCOMode = NoTOCOTransducer | ExternalTOCO | IUP | UnknownTOCOMode deriving (Show)

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

getCTGStatus :: CTGData -> CTGStatus
getCTGStatus  = get ctgStatus 

getHR1 :: CTGData -> [HR1]
getHR1 = get ctgHR1

getHR2 :: CTGData -> [HR2]
getHR2 = get ctgHR2

getMHR :: CTGData -> [MHR]
getMHR = get ctgMHR

getTOCO :: CTGData -> [TOCO]
getTOCO = get ctgToco

getHR1Mode :: CTGData -> HRMode
getHR1Mode = get ctgHR1Mode

getHR2Mode :: CTGData -> HRMode
getHR2Mode = get ctgHR2Mode

getMHRMode :: CTGData -> HRMode
getMHRMode = get ctgMHRMode

getTOCOMode :: CTGData -> TOCOMode
getTOCOMode = get ctgTocoMode

getFMPStatus :: CTGData -> Bool
getFMPStatus  ctgData = get fmpEnabled $  getCTGStatus ctgData

getHRTwinOffsetStatus :: CTGData -> Bool
getHRTwinOffsetStatus  ctgData = get hrTwinOffsetOn $ getCTGStatus ctgData

getDECGLogicStatus::CTGData -> Bool
getDECGLogicStatus ctgData  = get decgLogicOn $  getCTGStatus ctgData

getHRCrossChannelStatus :: CTGData -> Bool
getHRCrossChannelStatus ctgData  = get hrCrossChannelVer $  getCTGStatus ctgData

getTelemetryStatus :: CTGData -> Bool
getTelemetryStatus ctgData  = get telemetryOn $  getCTGStatus ctgData

getFSPO2AvailableStatus :: CTGData -> Bool
getFSPO2AvailableStatus ctgData  = get fspo2Available $  getCTGStatus ctgData

getDataDeletedStatus :: CTGData -> Bool
getDataDeletedStatus ctgData  = get ctgDataDeleted $  getCTGStatus ctgData

getDataInsertedStatus :: CTGData -> Bool
getDataInsertedStatus ctgData = get ctgDataInserted $  getCTGStatus ctgData

getMonitorOnStatus :: CTGData -> Bool
getMonitorOnStatus ctgData  = get monitorOn $  getCTGStatus ctgData

