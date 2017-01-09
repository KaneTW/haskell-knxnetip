{-# LANGUAGE UndecidableInstances #-}
module Network.KNX.IP.Types where

import Data.Kind
import Data.Word
import Data.Typeable

import Network.KNX.IP.Serialize

-- |Existential wrapper 
data Some :: (k -> *) -> * where
  Some :: f a -> Some f

newtype IndividualAddress = IndividualAddress Word16
  deriving (Show, Read, Eq, Ord, Serialize)

data KNXError
  = NoError
  | UnsupportedHostProtcol
  | VersionNotSupported
  | SequenceOutOfOrder
  deriving (Show, Read, Eq, Ord, Typeable)


data ServiceFamily
  = Core
  | DeviceManagement
  | Tunneling
  | Routing
  | RemoteLogging
  | RemoteConfig
  | ObjectServer
  deriving (Show, Read, Eq, Ord, Typeable)

instance Serialize ServiceFamily where
  put Core = putWord8 2
  put DeviceManagement = putWord8 3
  put Tunneling = putWord8 4
  put Routing = putWord8 5
  put RemoteLogging = putWord8 6
  put RemoteConfig = putWord8 7
  put ObjectServer = putWord8 8

  get = getWord8 >>= toSF
    where
      toSF 2 = pure Core
      toSF 3 = pure DeviceManagement
      toSF 4 = pure Tunneling
      toSF 5 = pure Routing
      toSF 6 = pure RemoteLogging
      toSF 7 = pure RemoteConfig
      toSF 8 = pure ObjectServer
      toSF _ = fail "Unknown service family"
