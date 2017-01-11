module Network.KNX.IP.Services.Core where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Word
import Data.Type.Equality
import Data.Typeable
import GHC.TypeLits
import Net.Types (IPv4, Mac)
import Network.KNX.IP.Serialize
import Network.KNX.IP.Types
import Language.Haskell.TH
import Network.KNX.IP.TH

data IPTransport = UDP | TCP
  deriving (Show, Read, Eq, Ord, Typeable)

mkEnumSerialize ''IPTransport [ ('UDP, 1)
                              , ('TCP, 2) ]

data HPAI where
  HPAI :: IPTransport -> IPv4 -> Word16 -> HPAI
  deriving (Show, Read, Eq, Ord, Typeable)

instance Serialize HPAI where
  put (HPAI proto addr port) = do
    putWord8 8
    put proto
    put addr
    putWord16be port

  get = isolate 8 $ do
    len <- getWord8
    when (len /= 8) $ fail "Incorrect HPAI length"
    HPAI <$> get <*> get <*> get

data KNXMedium
  = TP1
  | PL110
  | RF
  | KNXIP
  deriving (Show, Read, Eq, Ord, Typeable)

mkEnumSerialize ''KNXMedium [ ('TP1, 2)
                            , ('PL110, 4)
                            , ('RF, 0x10)
                            , ('KNXIP, 0x20) ]
type DeviceStatus = Word8
type KNXSerialNumber = Mac -- not really a mac but lazy atm


data DescriptionType
  = DeviceInfo
  | SupportedServiceFamilies
  | IPConfig
  | IPCurrentConfig
  | KNXAddresses
  | ManufacturerData
  deriving (Show, Read, Eq, Ord, Typeable)

  
data NulPadded (n :: Nat) where
  NulPadded :: ByteString -> NulPadded n

deriving instance Show (NulPadded n)
deriving instance Eq (NulPadded n)

nulPadded :: forall n. KnownNat n => ByteString -> Maybe (NulPadded n)
nulPadded str = NulPadded <$> valid (fromIntegral $ natVal (Proxy :: Proxy n))
  where
    valid n | BS.length str >= n = Nothing
            | 0 `BS.elem` str = Nothing
            | otherwise = Just str
                    

instance KnownNat n => Serialize (NulPadded n) where
  put (NulPadded str) = putByteString padded
    where
      padded = str `BS.append` BS.replicate (n - BS.length str) 0
      n = fromIntegral $ natVal (Proxy :: Proxy n)
      
  get = NulPadded . BS.takeWhile (>0) <$> getByteString len
    where
      len = fromIntegral $ natVal (Proxy :: Proxy n)
    
    
data DIB (a :: DescriptionType) where
  MkDeviceInfo :: {
    deviceKnxMedium :: KNXMedium,
    deviceStatus :: DeviceStatus,
    deviceKnxIndividual :: IndividualAddress,
    devicePIID :: Word16,
    deviceKnxSerialNumber :: KNXSerialNumber,
    deviceRoutingMulticastAddress :: IPv4,
    deviceMacAddress :: Mac,
    deviceFriendlyName :: NulPadded 30 -- length 30, nul terminated
    } -> DIB 'DeviceInfo

  MkSupportedServiceFamilies :: {
    supportedServiceFamilies :: [(ServiceFamily, Word8)]
    } -> DIB 'SupportedServiceFamilies

  MkIPConfig :: {
    ipConfigAddress :: IPv4,
    ipConfigSubmask :: IPv4,
    ipConfigGateway :: IPv4,
    ipConfigCapabilities :: Word8,
    ipConfigAssignmentMethod :: Word8
    } -> DIB 'IPConfig

  MkIPCurrentConfig :: {
    ipCurrentAddress :: IPv4,
    ipCurrentSubmask :: IPv4,
    ipCurrentGateway :: IPv4,
    ipCurrentDHCPServer :: IPv4,
    ipCurrentAssignmentMethod :: Word8
    } -> DIB 'IPCurrentConfig

  MkKNXAddresses :: {
    knxAddresses :: NonEmpty IndividualAddress
    } -> DIB 'KNXAddresses

  MkManufacturerData :: {
    manufacturerId :: Word16,
    manufacturerData :: ByteString
    } -> DIB 'ManufacturerData


deriving instance Show (DIB a)
deriving instance Eq (DIB a)

return []

instance TestEquality DIB where
 testEquality a b
   = $(do
          TyConI (DataD _ _ _ _ cs _) <- reify ''DIB
          let names = let toNames (GadtC ns _ _) = ns
                          toNames (RecGadtC ns _ _) = ns
                          toNames _ = []
                      in concatMap toNames cs
                                    
          let matches = let mkMatch name =
                              match (tupP [recP name [], recP name []]) (normalB [|Just Refl|]) []
                        in map mkMatch names ++ [match wildP (normalB [|Nothing|]) []]
          caseE [|(a, b)|] matches
      )


 
instance Serialize (DIB 'DeviceInfo) where
  put MkDeviceInfo {..} = putTagged 1 $ do
    put deviceKnxMedium
    put deviceStatus
    put deviceKnxIndividual
    putWord16be devicePIID
    put deviceKnxSerialNumber
    put deviceRoutingMulticastAddress
    put deviceMacAddress
    put deviceFriendlyName


  get = getTagged 1 $ MkDeviceInfo <$> get
    <*> get <*> get <*> getWord16be
    <*> get <*> get <*> get
    <*> get
    

instance Serialize (DIB 'SupportedServiceFamilies) where
  put MkSupportedServiceFamilies {..} = putTagged 2 $ mapM_ put supportedServiceFamilies

  get = getTagged 2 $ MkSupportedServiceFamilies <$> many get 

instance Serialize (DIB 'IPConfig) where
  put MkIPConfig {..} = putTagged 3 $ do
    put ipConfigAddress
    put ipConfigSubmask
    put ipConfigGateway
    put ipConfigCapabilities
    put ipConfigAssignmentMethod

  get = getTagged 3 $ MkIPConfig <$> get
    <*> get <*> get <*> get <*> get

instance Serialize (DIB 'IPCurrentConfig) where
  put MkIPCurrentConfig {..} = putTagged 4 $ do
    put ipCurrentAddress
    put ipCurrentSubmask
    put ipCurrentGateway
    put ipCurrentDHCPServer
    put ipCurrentAssignmentMethod
    putWord8 0

  get = getTagged 4 $ MkIPCurrentConfig <$> get
        <*> get <*> get <*> get <*> get <* getWord8

instance Serialize (DIB 'KNXAddresses) where
  put MkKNXAddresses {..} = putTagged 5 $ do
    mapM_ put knxAddresses

  get = getTagged 5 $ do
    first <- get
    rest <- many get
    pure $ MkKNXAddresses (first :| rest)

instance Serialize (DIB 'ManufacturerData) where
  put MkManufacturerData {..} = putTagged 0xfe $ do
    put manufacturerId
    putByteString manufacturerData

  get = getTagged 0xfe $ do
    mId <- get
    n <- remaining
    mData <- getByteString n
    pure $ MkManufacturerData mId mData


instance Serialize (Some DIB) where
  put (Some x@MkDeviceInfo {}) = put x
  put (Some x@MkSupportedServiceFamilies {}) = put x
  put (Some x@MkIPConfig {}) = put x
  put (Some x@MkIPCurrentConfig {}) = put x
  put (Some x@MkKNXAddresses {}) = put x
  put (Some x@MkManufacturerData {}) = put x
  
  get = (Some <$> (get :: Get (DIB 'DeviceInfo)))
        <|> (Some <$> (get :: Get (DIB 'SupportedServiceFamilies)))
        <|> (Some <$> (get :: Get (DIB 'IPConfig)))
        <|> (Some <$> (get :: Get (DIB 'IPCurrentConfig)))
        <|> (Some <$> (get :: Get (DIB 'KNXAddresses)))
        <|> (Some <$> (get :: Get (DIB 'ManufacturerData)))
        <|> fail "Unknown DIB"


deriving instance Show (Some DIB)


instance Eq (Some DIB) where
  Some a == Some b
    = case testEquality a b of
        Just Refl -> a == b
        _ -> False


data SearchRequest where
  SearchRequest :: HPAI -> SearchRequest
  deriving (Show, Eq, Typeable)

instance Serialize SearchRequest where
  put (SearchRequest h) = putWithHeader 0x201 (put h)
  get = getWithHeader 0x201 (SearchRequest <$> get)


data SearchResponse where
  SearchResponse :: HPAI -> DIB 'DeviceInfo
                 -> DIB 'SupportedServiceFamilies -> SearchResponse
  deriving (Show, Eq, Typeable)

instance Serialize SearchResponse where
  put (SearchResponse h di ssf) = putWithHeader 0x202 (put h >> put di >> put ssf)
  get = getWithHeader 0x202 (SearchResponse <$> get <*> get <*> get)



data DescriptionRequest where
  DescriptionRequest :: HPAI -> DescriptionRequest
  deriving (Show, Eq, Typeable)

instance Serialize DescriptionRequest where
  put (DescriptionRequest h) = putWithHeader 0x203 (put h)
  get = getWithHeader 0x203 (DescriptionRequest <$> get)



data DescriptionResponse where
  DescriptionResponse :: DIB 'DeviceInfo -> DIB 'SupportedServiceFamilies
                      -> [Some DIB] -> DescriptionResponse
  deriving (Show, Eq, Typeable)

instance Serialize DescriptionResponse where
  put (DescriptionResponse di ssf dibs) = putWithHeader 0x204 $ put di >> put ssf >> mapM_ put dibs
  get = getWithHeader 0x204 $ DescriptionResponse <$> get <*> get <*> many get



data ConnectionType
  = DeviceMgmtConn
  | TunnelConn
  | RemoteLogConn 
  | RemoteConfConn
  | ObjectServerConn
  deriving (Show, Read, Eq, Ord, Typeable)


data KNXLayer
  = LinkLayerTunnel
  | RawTunnel
  | BusmonitorTunnel
  deriving (Show, Read, Eq, Ord, Typeable)

mkEnumSerialize ''KNXLayer [ ('LinkLayerTunnel, 2)
                           , ('RawTunnel, 4)
                           , ('BusmonitorTunnel, 0x80) ]

data CRI (a :: ConnectionType) where
  DeviceMgmtCRI :: CRI 'DeviceMgmtConn
  TunnelCRI :: KNXLayer -> CRI 'TunnelConn

deriving instance Show (CRI a)
deriving instance Eq (CRI a)

instance Serialize (CRI 'DeviceMgmtConn) where
  put DeviceMgmtCRI = putTagged 3 (return ())
  get = getTagged 3 (pure DeviceMgmtCRI)

instance Serialize (CRI 'TunnelConn) where
  put (TunnelCRI l) = putTagged 4 (put l >> putWord8 0)
  get = getTagged 4 (TunnelCRI <$> get <* getWord8)

  
data CRD (a :: ConnectionType) where
  DeviceMgmtCRD :: CRD 'DeviceMgmtConn
  TunnelCRD :: IndividualAddress -> CRD 'TunnelConn

deriving instance Show (CRD a)
deriving instance Eq (CRD a)
  
instance Serialize (CRD 'DeviceMgmtConn) where
  put DeviceMgmtCRD = putTagged 3 (return ())
  get = getTagged 3 (pure DeviceMgmtCRD)

instance Serialize (CRD 'TunnelConn) where
  put (TunnelCRD addr) = putTagged 4 (put addr)
  get = getTagged 4 (TunnelCRD <$> get)





data ConnectRequest a where
  ConnectRequest :: HPAI -> HPAI -> CRI a -> ConnectRequest a

deriving instance Show (ConnectRequest a)
deriving instance Eq (ConnectRequest a)

instance Serialize (CRI a) => Serialize (ConnectRequest a) where
  put (ConnectRequest h1 h2 cri) = putWithHeader 0x205 $ put h1 >> put h2 >> put cri
  get = getWithHeader 0x205 $ ConnectRequest <$> get <*> get <*> get

newtype CommChannel = CommChannel Word8
  deriving (Show, Read, Eq, Ord, Typeable, Serialize)


data ConnectResponse (a :: ConnectionType) where
  ConnectResponse :: CommChannel -> KNXError -> HPAI -> CRD a -> ConnectResponse a

deriving instance Show (ConnectResponse a)
deriving instance Eq (ConnectResponse a)

instance Serialize (CRD a) => Serialize (ConnectResponse a) where
  put (ConnectResponse cid cs h crd) = putWithHeader 0x206 $ put cid >> put cs >> put h >> put crd
  get = getWithHeader 0x206 $ ConnectResponse <$> get <*> get <*> get <*> get


data ConnectionStateRequest where
  ConnectionStateRequest :: CommChannel -> HPAI -> ConnectionStateRequest

deriving instance Show ConnectionStateRequest
deriving instance Eq ConnectionStateRequest

instance Serialize ConnectionStateRequest where
  put (ConnectionStateRequest ch h) = putWithHeader 0x207 $ put ch >> putWord8 0 >> put h
  get = getWithHeader 0x207 $ ConnectionStateRequest <$> get <* getWord8 <*> get


data ConnectionStateResponse where
  ConnectionStateResponse :: CommChannel -> KNXError -> ConnectionStateResponse

deriving instance Show ConnectionStateResponse
deriving instance Eq ConnectionStateResponse

instance Serialize ConnectionStateResponse where
  put (ConnectionStateResponse ch s) = putWithHeader 0x208 $ put ch >> put s
  get = getWithHeader 0x208 $ ConnectionStateResponse <$> get <*> get


data DisconnectRequest where
  DisconnectRequest :: CommChannel -> HPAI -> DisconnectRequest

deriving instance Show DisconnectRequest
deriving instance Eq DisconnectRequest

instance Serialize DisconnectRequest where
  put (DisconnectRequest ch h) = putWithHeader 0x209 $ put ch >> putWord8 0 >> put h
  get = getWithHeader 0x209 $ DisconnectRequest <$> get <* getWord8 <*> get


data DisconnectResponse where
  DisconnectResponse :: CommChannel -> KNXError -> DisconnectResponse

deriving instance Show DisconnectResponse
deriving instance Eq DisconnectResponse

instance Serialize DisconnectResponse where
  put (DisconnectResponse ch s) = putWithHeader 0x20a $ put ch >> put s
  get = getWithHeader 0x20a $ DisconnectResponse <$> get <*> get
