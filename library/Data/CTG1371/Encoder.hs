module Data.CTG1371.Encoder (encodeCTG)  where

import Data.CTG1371.Internal.Types
import Data.CTG1371.Internal.Encoder.Encoders
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P


-- | Provides mechanism for encoding an instance of CTGData into a ByteString 
encodeCTG :: CTGData -> BL.ByteString
encodeCTG ctgData =  P.runPut (buildCTGByteString ctgData)
