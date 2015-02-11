module Data.CTG1371.Parser (ctgParser
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

import Data.CTG1371.Internal.Types
import Data.CTG1371.Internal.Parser.Parsers
import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.Get as G
import Data.Monoid


{-| This is the primary parser.  It will parse a 1371 encoded "C" block bytestring into a
data cardiotocography structure. An exception is thrown if the bytestring is not a "C" block.
-}
ctgParser::BS.ByteString -> CTGData
ctgParser ctgdata =do
  let (parsedData,_)  = G.runGet parseCTG ctgdata
  case parsedData of
    Left errMsg -> error  "ctgParseError: " (mappend errMsg)
    Right x -> x
