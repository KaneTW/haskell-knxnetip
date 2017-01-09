{-# LANGUAGE OverloadedStrings #-}
module Network.KNX.IP.Services.CoreSpec where

import qualified Data.ByteString as BS
import Test.Import
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac
import Network.KNX.IP.Serialize 

spec :: Spec
spec = do
  describe "SearchRequest" $ do
    it "should parse the example correctly" $ do
      Right ex <- pure . runGet get $ BS.pack [6,0x10,2,1,0,0xe,8,1,192,168,200,12,0xe,0x57]
      ex `shouldBe` SearchRequest (HPAIUDP (IPv4.fromOctets 192 168 200 12) 3671)
      
  describe "SearchResponse" $ do
    it "should parse the example correctly" $ do
      Right ex <- pure . runGet get $ BS.pack [6,0x10,2,2,0,0x4e,8,1,192,168,200,12,0xc3,0xb4
                                              ,0x36,1,2,1,0x11,0,0,0x11,0,1,0x11,0x11,0x11,0x11
                                              ,224,0,23,12,0x45,0x49,0x42,0x6e,0x65,0x74
                                              ,77,89,72,79,77,69,0,0,0,0,0,0,0,0,0,0,0,0
                                              ,0,0,0,0,0,0,0,0,0,0,0,0,0xa,2,2,1,3,1,4,1
                                              ,5,1]
      Just name <- pure $ nulPadded "MYHOME"
      ex `shouldBe` SearchResponse (HPAIUDP (IPv4.fromOctets 192 168 200 12) 50100) 
        (MkDeviceInfo TP1 1 (IndividualAddress 0x1100) 0x11
        (Mac.fromOctets 0 1 0x11 0x11 0x11 0x11) (IPv4.fromOctets 224 0 23 12)
        (Mac.fromOctets 0x45 0x49 0x42 0x6e 0x65 0x74) name)

        (MkSupportedServiceFamilies [(Core, 1)
                                    ,(DeviceManagement, 1)
                                    ,(Tunneling, 1)
                                    ,(Routing, 1)])
  --TODO: rest
        
