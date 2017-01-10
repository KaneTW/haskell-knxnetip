module Network.KNX.IP where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Monad.State as S
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.STM
import qualified Data.ByteString as BS
import Data.Typeable
import qualified Net.IPv4 as IPv4
import qualified Net.IPv4.String as IPv4S
import Net.Types
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.KNX.IP.Serialize
import Network.KNX.IP.Types
import Network.KNX.IP.Services.Core
import System.Timeout

data DiscoverySettings = DiscoverySettings {
  discoveryHost :: IPv4,
  discoveryPort :: PortNumber,
  discoveryTimeout :: Int }
  deriving (Show, Eq, Ord, Typeable)

defaultDiscoverySettings :: DiscoverySettings
defaultDiscoverySettings = DiscoverySettings (IPv4.fromOctets 224 0 23 12) 3671 (2*1000*1000)

data KNXState = KNXState {
  }
  deriving (Show, Eq, Ord, Typeable)

newtype KNXM a = KNXM (S.StateT KNXState IO a)
  deriving ( Functor, Applicative, Monad, S.MonadIO)

hostAddrToIPv4 :: HostAddress -> IPv4
hostAddrToIPv4 addr = let (a,b,c,d) = hostAddressToTuple addr
                      in IPv4.fromOctets a b c d

sockAddrToHPAI :: Socket -> SockAddr -> HPAI
sockAddrToHPAI (MkSocket _ _ Stream _ _) (SockAddrInet port addr) = HPAITCP (hostAddrToIPv4 addr) (fromIntegral port)
sockAddrToHPAI (MkSocket _ _ Datagram _ _) (SockAddrInet port addr) = HPAIUDP (hostAddrToIPv4 addr) (fromIntegral port)

--TODO: leaking sockets
discoverServers :: DiscoverySettings -> IO [SearchResponse]
discoverServers DiscoverySettings{..} = runResourceT $ do
  (_, recvr) <- allocate (socket AF_INET Datagram defaultProtocol) close
  (_, sock) <- allocate (socket AF_INET Datagram defaultProtocol) close
  liftIO $ do
    bind recvr (SockAddrInet aNY_PORT (tupleToHostAddress (10,1,1,13)))
    recvAddr <- getSocketName recvr
    
    response <- newTVarIO []
    progress <- newTVarIO initial
    
    lock <- newEmptyMVar
    tid <- forkFinally (timeout discoveryTimeout $ getResponses recvr response progress BS.empty) (cleanUp response progress lock)
    
    let addr = SockAddrInet 3671 (tupleToHostAddress (10,1,5,136))
    sendAllTo sock (runPut . put . SearchRequest $ sockAddrToHPAI recvr recvAddr) addr

    takeMVar lock
    atomically $ readTVar response

  where
    initial = runGetChunk (get :: Get SearchResponse) Nothing BS.empty

    cleanUp response progress lock _ = do
      res <- atomically $ readTVar progress
      case res of
        Partial f -> do
          let (Done r _) = f BS.empty
          atomically $ modifyTVar' response (r:)
        _ -> return ()

      putMVar lock ()
    
    getResponses sock response progress prev
      = atomically (readTVar progress) >>= feedResult
      where
        --TODO: no way to distinguish between proper parse errors and anything else
        feedResult (Fail err _) = ioError (userError err)
        feedResult x@(Partial f) = do
          atomically $ swapTVar progress x
          (bs, _) <- recvFrom sock 1024
          feedResult (f bs)
        feedResult (Done r bs) = do
          atomically $ do
            swapTVar progress initial
            modifyTVar' response (r:)
          getResponses sock response progress bs
      
    emptyChan chan = do
      ele <- readTChan chan
      (ele:) <$> emptyChan chan



