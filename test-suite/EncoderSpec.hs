module EncoderSpec  where

import Test.Hspec
import Data.CTG1371.Internal.Encoder.Encoders
import Data.CTG1371.Internal.Types
import Data.Word
import Data.List (foldl')
import qualified Data.ByteString as BS

binToWord16 :: String -> Word16
binToWord16 = foldl' (\acc x -> acc * 2 + digToWord16 x) 0
  where digToWord16 c = case c of
                          '0' -> 0
                          '1' -> 1
                          _   -> error "Invalid digit"

spec ::Spec
spec = describe "Test Encoder Internal Functions" $ do
         it "test packWord8 all true" $ do
           packWord8 (True,True,True,True,True,True,True,True) `shouldBe` 0xFF
           
         it "test packWord8 all false" $ do
           packWord8 (False,False,False,False,False,False,False,False) `shouldBe` 0

         it "test encodeHR for hr=100 fm == movement and Green signal quality" $ do
           encodeHR 100 Movement SignalGreen `shouldBe` binToWord16 "0100100110010000"

         it "test encodeHR hr = 256 fm = no movement and Red signal quality" $ do
           encodeHR 256 NoMovement SignalRed `shouldBe` binToWord16 "0000010000000000"

         it "test encodeHR  for hr = 256 fm = movement and Yellow signal quality" $ do
           encodeHR 256 Movement SignalYellow `shouldBe` binToWord16 "0010110000000000"

         it "test packHR1 for hr = 256 fm = movement and Yellow signal quality" $ do
           packHR1 (buildHR1 Movement SignalYellow  256 False) [] `shouldBe` [44,0]

         it "test packHR1 for hr = 100 fm = no movement and Red signal quality" $ do
           packHR1 (buildHR1 NoMovement SignalRed 100 False) [] `shouldBe` [1,144]
                          
         it "test encodeHR1 for the correct order of HR values" $ do
           encodeHR1 [ ( buildHR1 NoMovement SignalRed 100 False)
                      ,( buildHR1 NoMovement SignalRed 101 False)
                      ,( buildHR1 NoMovement SignalRed 102 False)
                      ,( buildHR1 NoMovement SignalRed 103 False)
                     ] `shouldBe` BS.pack [1,156,1,152,1,148,1,144]
         
