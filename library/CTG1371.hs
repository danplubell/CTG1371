{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | TODO
module DATA.CTG1371.Parser  where

-- HASKELETON: import New.Module as CTG1371

-- | TODO
data CTGStatus = CTGStatus { monitorOn::Bool
                           , ctgDataInserted::Bool
                           , ctgDataDeleted::Bool
                           , fsp02Available::Bool
                           , telemetryStatus::Bool
                           , hrCrossChannelVer::Bool
                           , decgLogic::Bool
                           , ht1TwinOffset::Bool
                           , fmpStatus::Bool
                           } deriving (Show)
                                      
data SignalQuality = Red | Yellow | Green deriving (Show)

data FetalMovement = Movement | NoMovement deriving (Show)

data HR = HR { signalQuality::SignalQuality
             , heartRate::Int
             } deriving (Show)
                        
data HR1 = HR1 { fmp::FetalMovement
               , hr1::HR  
               } deriving (Show) 

data HR2 = HR2 { hr2::HR } deriving (Show)

data MHR = MHR { mhr::HR } deriving (Show)

data TOCO = TOCO { tocoRate::Int } deriving (Show)

data HRMode = NoHRTransducer | US | DECG | MECG | ExternalMRH | UnknownHRMode deriving (Show)

data TOCOMode = NoTOCOTransducer | TOCOExternal | IUP | UnknownTOCOMode deriving (Show)

data CTGData = CTGData { ctgStatus::CTGStatus
                       , ctgHR1::[HR1]
                       , ctgHR2::[HR2]
                       , ctgMHR::[MHR]
                       , ctgToco::[TOCO]
                       , ctgHRMode::HRMode
                       , ctgTocoMode::TOCOMode
                       } deriving (Show)
