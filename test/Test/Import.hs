{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Import (module M) where


import qualified Data.ByteString as BS
import Data.List.NonEmpty as M (NonEmpty(..))
import Data.DeriveTH as M
import Data.Proxy
import Data.Word as M
import GHC.TypeLits

import Test.QuickCheck as M
import Test.QuickCheck.Checkers as M
import Test.Hspec as M
import Net.Types as M
import Network.KNX.IP.Serialize as M (Serialize(..))
import Network.KNX.IP.Types as M
import Network.KNX.IP.Services.Core as M
import Network.KNX.IP.Services.DeviceManagement as M
import Network.KNX.IP.Services.Tunneling as M

instance Eq a => EqProp a where (=-=) = eq

derive makeArbitrary ''Mac
derive makeArbitrary ''IPv4
derive makeArbitrary ''KNXError
derive makeArbitrary ''ServiceFamily
derive makeArbitrary ''IndividualAddress
derive makeArbitrary ''KNXMedium
derive makeArbitrary ''NonEmpty
derive makeArbitrary ''IPTransport
derive makeArbitrary ''HPAI


instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary

instance KnownNat n => Arbitrary (NulPadded n) where
  arbitrary = do
    len <- fromIntegral <$> choose (0, (natVal (Proxy :: Proxy n)) - 1)
    Just res <- nulPadded . BS.pack <$> vectorOf len (arbitrary `suchThat` (>0))
    return res

instance Arbitrary (DIB 'DeviceInfo) where
  arbitrary = MkDeviceInfo <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary (DIB 'SupportedServiceFamilies) where
  arbitrary = MkSupportedServiceFamilies <$> arbitrary

instance Arbitrary (DIB 'IPConfig) where
  arbitrary = MkIPConfig <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary (DIB 'IPCurrentConfig) where
  arbitrary = MkIPCurrentConfig  <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary (DIB 'KNXAddresses) where
  arbitrary = MkKNXAddresses <$> arbitrary

instance Arbitrary (DIB 'ManufacturerData) where
  arbitrary = MkManufacturerData <$> arbitrary <*> arbitrary


instance Arbitrary (Some DIB) where
  arbitrary = oneof [
    Some <$> (arbitrary :: Gen (DIB 'DeviceInfo))
    , Some <$> (arbitrary :: Gen (DIB 'SupportedServiceFamilies))
    , Some <$> (arbitrary :: Gen (DIB 'IPConfig))
    , Some <$> (arbitrary :: Gen (DIB 'IPCurrentConfig))
    , Some <$> (arbitrary :: Gen (DIB 'KNXAddresses))
    , Some <$> (arbitrary :: Gen (DIB 'ManufacturerData))
    ]

derive makeArbitrary ''SearchRequest
derive makeArbitrary ''SearchResponse
derive makeArbitrary ''DescriptionRequest
derive makeArbitrary ''DescriptionResponse

derive makeArbitrary ''KNXLayer

instance Arbitrary (CRI 'DeviceMgmtConn) where
  arbitrary = pure DeviceMgmtCRI

instance Arbitrary (CRI 'TunnelConn) where
  arbitrary = TunnelCRI <$> arbitrary

instance Arbitrary (CRD 'DeviceMgmtConn) where
  arbitrary = pure DeviceMgmtCRD
  
instance Arbitrary (CRD 'TunnelConn) where
  arbitrary = TunnelCRD <$> arbitrary

instance Arbitrary (CRI a) => Arbitrary (ConnectRequest a) where
  arbitrary = ConnectRequest <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (CRD a) => Arbitrary (ConnectResponse a) where
  arbitrary = ConnectResponse <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary

  
derive makeArbitrary ''CommChannel

derive makeArbitrary ''ConnectionStateRequest
derive makeArbitrary ''ConnectionStateResponse
derive makeArbitrary ''DisconnectRequest
derive makeArbitrary ''DisconnectResponse
