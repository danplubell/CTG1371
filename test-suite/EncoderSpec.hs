module EncoderSpec  where

import Test.Hspec
import Data.CTG1371.Internal.Encoder.Encoders
import Data.CTG1371.Internal.Types
import Data.Word
import Data.List (foldl')

binToWord16 :: String -> Word16
binToWord16 = foldl' (\acc x -> acc * 2 + digToWord16 x) 0
  where digToWord16 c = case c of
                          '0' -> 0
                          '1' -> 1
                          _   -> error "Invalid digit"

spec ::Spec
spec = describe "Test Encoder Internal Functions" $ do
         it "packWord8 all true" $ do
           packWord8 (True,True,True,True,True,True,True,True) `shouldBe` 0xFF
           
         it "packWord8 all false" $ do
           packWord8 (False,False,False,False,False,False,False,False) `shouldBe` 0

         it "pack an HR for hr=100 fm == movement and Green signal quality" $ do
           encodeHR 100 Movement SignalGreen `shouldBe` binToWord16 "0100100110010000"

         it "pack an HR for hr = 256 fm = no movement and Red signal quality" $ do
           encodeHR 256 NoMovement SignalRed `shouldBe` binToWord16 "0000010000000000"

         it "pack an HR for hr = 256 fm = movement and Yellow signal quality" $ do
           encodeHR 256 Movement SignalYellow `shouldBe` binToWord16 "0010110000000000"
           
--         it "packHR1 for heart rate of 100, NoFetalMovement, SignalGreen " $ do
--           packHR1 (buildHR1 Movement SignalGreen 100 False) [] `shouldBe` [0::Word8] 



                          
