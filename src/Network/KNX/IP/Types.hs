module Network.KNX.IP.Types where

import Data.Kind
import Data.Word
import Data.Typeable

import Network.KNX.IP.Serialize
import Network.KNX.IP.TH

-- |Existential wrapper 
data Some :: (k -> *) -> * where
  Some :: f a -> Some f

newtype IndividualAddress = IndividualAddress Word16
  deriving (Show, Read, Eq, Ord, Serialize)

data KNXError
  = NoError
  | UnsupportedHostProtocol
  | VersionNotSupported
  | SequenceOutOfOrder
  | UnknownConnectionID
  | InvalidConnectionType
  | InvalidConnectionOptions
  | NoMoreConnections
  | DataConnectionError
  | KNXConnectionError
  | InvalidTunnelingLayer
  deriving (Show, Read, Eq, Ord, Typeable)

mkEnumSerialize ''KNXError [ ('NoError, 0)
                             , ('UnsupportedHostProtocol, 1)
                             , ('VersionNotSupported, 2)
                             , ('SequenceOutOfOrder, 4)
                             , ('UnknownConnectionID, 0x21)
                             , ('InvalidConnectionType, 0x22)
                             , ('InvalidConnectionOptions, 0x23)
                             , ('NoMoreConnections, 0x24)
                             , ('DataConnectionError, 0x26)
                             , ('KNXConnectionError, 0x27)
                             , ('InvalidTunnelingLayer, 0x29) ]

data ServiceFamily
  = Core
  | DeviceManagement
  | Tunneling
  | Routing
  | RemoteLogging
  | RemoteConfig
  | ObjectServer
  deriving (Show, Read, Eq, Ord, Typeable)

mkEnumSerialize ''ServiceFamily [ ('Core, 2)
                                , ('DeviceManagement, 3)
                                , ('Tunneling, 4)
                                , ('Routing, 5)
                                , ('RemoteLogging, 6)
                                , ('RemoteConfig, 7)
                                , ('ObjectServer, 8) ]
