module Network.KNX.IP where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.STM
import qualified Data.ByteString as BS
import Data.Typeable
import qualified Net.IPv4 as IPv4
import qualified Net.IPv4.String as IPv4S
import Net.Types
import Network.Multicast
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Network.KNX.IP.Serialize as S
import Network.KNX.IP.Types
import Network.KNX.IP.Services.Core
import System.Timeout

data KNXSettings = KNXSettings {
  bindMulticast :: Bool,
  bindHost :: IPv4,
  bindPort :: PortNumber,
  discoveryMulticast :: Bool,
  discoveryHost :: IPv4,
  discoveryPort :: PortNumber,
  discoveryTimeout :: Int }
  deriving (Show, Eq, Ord, Typeable)

defaultKNXSettings :: KNXSettings
defaultKNXSettings = KNXSettings {
  bindMulticast = True,
  bindHost = IPv4.fromOctets 224 0 23 12,
  bindPort = 3671,
  discoveryMulticast = True,
  discoveryHost = IPv4.fromOctets 224 0 23 12,
  discoveryPort = 3671,
  discoveryTimeout = 2*1000*1000
  }

data KNXState = KNXState {
  serverControlEndpoint :: HPAI
  }
  deriving (Show, Eq, Ord, Typeable)

type MonadKNX m = (MonadResourceBase m, MonadReader KNXSettings m, MonadState KNXState m)

hostAddrToIPv4 :: HostAddress -> IPv4
hostAddrToIPv4 addr = let (a,b,c,d) = hostAddressToTuple addr
                      in IPv4.fromOctets a b c d

iPv4ToHostAddr :: IPv4 -> HostAddress
iPv4ToHostAddr = tupleToHostAddress . IPv4.toOctets

sockAddrToHPAI :: Socket -> SockAddr -> HPAI
sockAddrToHPAI (MkSocket _ _ proto _ _) (SockAddrInet port addr) = HPAI proto' (hostAddrToIPv4 addr) (fromIntegral port)
  where
    proto' = case proto of
      Stream -> TCP
      Datagram -> UDP
      _ -> error "Unsupported socket"

hPAIToSockAddr :: HPAI -> SockAddr
hPAIToSockAddr (HPAI _ addr port) = SockAddrInet (fromIntegral port) (iPv4ToHostAddr addr)

mkSocketFromHPAI :: HPAI -> IO (Socket, SockAddr)
mkSocketFromHPAI h@(HPAI proto addr port) = do
  let sockAddr = hPAIToSockAddr h
  sock <- case proto of
    TCP -> do
      sock <- getProtocolNumber "tcp" >>= socket AF_INET Stream
      connect sock sockAddr
      return sock
    UDP -> getProtocolNumber "udp" >>= socket AF_INET Datagram
  return (sock, sockAddr)



--TODO: outer monad
discoverServers :: KNXSettings -> IO [SearchResponse]
discoverServers KNXSettings{..} = runResourceT $ do
  (_, recvr) <- allocate mkReceiverSocket close
  (_, (sock, addr)) <- allocate mkSenderSocket (close . fst)
  liftIO $ do
    recvAddr <- getSocketName recvr
    
    response <- newTVarIO []
    lock <- newEmptyMVar
    
    forkFinally (timeout discoveryTimeout $ getResponses recvr response) (cleanUp lock)

    let req = SearchRequest $ sockAddrToHPAI recvr recvAddr

    sendAllTo sock (S.runPut $ S.put req) addr

    takeMVar lock
    atomically $ readTVar response

  where
    mkReceiverSocket
      | bindMulticast
      = multicastReceiver (IPv4S.encode bindHost) bindPort
      | otherwise
      = do
          sock <- getProtocolNumber "udp" >>= socket AF_INET Datagram
          bind sock (SockAddrInet bindPort $ iPv4ToHostAddr bindHost)
          return sock
    mkSenderSocket
      | discoveryMulticast
      = multicastSender (IPv4S.encode discoveryHost) discoveryPort
      | otherwise
      = mkSocketFromHPAI (HPAI UDP discoveryHost (fromIntegral discoveryPort))
    
    cleanUp lock _ = do
      putMVar lock ()

    -- UDP only. TCP requires stream handling
    getResponses sock response = do
      (bs, _) <- recvFrom sock 2048
      case S.runGet S.get bs of
        Right r -> atomically $ modifyTVar' response (r:)
        Left err -> return ()
        
      getResponses sock response

describeServer :: SearchResponse -> IO DescriptionResponse
describeServer (SearchResponse h _ _) = do
  (sender, addr) <- mkSocketFromHPAI h

  recvr <- getProtocolNumber "udp" >>= socket AF_INET Datagram
  bind recvr (SockAddrInet aNY_PORT 0)
  recvrAddr <- getSocketName recvr

  let req = DescriptionRequest $ sockAddrToHPAI recvr recvrAddr
  sendAllTo sender (S.runPut $ S.put req) addr

  --TODO: timeout
  (bs, _) <- recvFrom recvr 2048
  case S.runGet S.get bs of
    Right r -> return r
    Left err -> ioError (userError err)

