module Network.KNX.IP.Serialize
  ( Serialize(..),
    putWithHeader,
    getWithHeader,
    putTagged,
    getTagged,
    getTagged',
    module M
  ) where

import Control.Monad
import Data.Word
import Net.Types (IPv4(..), Mac(..))

import Data.Serialize as M hiding (Serialize(..))

class Serialize a where
  put :: Putter a
  get :: Get a
  

putWithHeader :: Word16 -> Put -> Put
putWithHeader sid = putNested putHeader
  where
    putHeader len = do
      putWord8 6
      putWord8 0x10
      putWord16be sid
      putWord16be (fromIntegral len + 6)

getWithHeader :: Word16 -> Get a -> Get a
getWithHeader sid = getNested getHeader
  where
    getHeader = do
      headerLength <- getWord8
      when (headerLength /= 6) $ fail "Incorrect header length"
      headerVersion <- getWord8
      when (headerVersion /= 0x10) $ fail "Incorrect header version"
      headerServiceId <- getWord16be
      when (headerServiceId /= sid) $ fail "Incorrect service id"
      headerTotalLength <- getWord16be
      pure $ fromIntegral headerTotalLength - fromIntegral headerLength


putTagged :: Word8 -> Put -> Put
putTagged tag = putNested putTag
  where
    putTag len = do
      putWord8 (fromIntegral len + 2)
      putWord8 tag

getTagged :: Word8 -> Get a -> Get a
getTagged tag = getNested getTag
  where
    getTag = do
      len <- getWord8
      rtag <- getWord8
      when (tag /= rtag) $ fail "Incorrect tag"
      return (fromIntegral len - 2)

getTagged' :: (Word8 -> Get a) -> Get a
getTagged' m = do
  len <- fromIntegral <$> getWord8 
  tag <- getWord8
  isolate (len - 2) (m tag)

  

instance Serialize Word8 where
  put = putWord8
  get = getWord8

instance Serialize Word16 where
  put = putWord16be
  get = getWord16be
  
instance Serialize Word32 where
  put = putWord32be
  get = getWord32be

instance Serialize Word64 where
  put = putWord64be
  get = getWord64be

instance Serialize IPv4 where
  put (IPv4 w) = putWord32be w
  get = IPv4 <$> getWord32be

instance Serialize Mac where
  put (Mac a b) = putWord16be a *> putWord32be b
  get = Mac <$> getWord16be <*> getWord32be

instance (Serialize a, Serialize b) => Serialize (a, b) where
  put = putTwoOf put put
  get = getTwoOf get get
  
