module Data.CTG1371 (CTGStatus(..)
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
                       }  deriving (Show)

