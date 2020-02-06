module TByteVector
	( TByteVector
	, newTByteVector
	, append
	, read
	) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.ByteString as B hiding (append)
import Data.Text as T hiding (append)
import Data.Text.Encoding
import Data.Vector.Unboxed.Mutable (IOVector(..))
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word
import GHC.Conc
import Prelude hiding (read)

data TByteVector = TByteVector
	{ usedLength :: TVar Word
	, uVector :: TVar (IOVector Word8)
	}

newTByteVector :: IO TByteVector
newTByteVector = do
	ic <- newTVarIO 0
	vec <- V.unsafeNew 0
	v <- newTVarIO vec
	return $ TByteVector ic v

writeList :: IOVector Word8 -> Int -> [Word8] -> IO ()
writeList vec _ [] = return ()
writeList vec offset (x:xs) = do
		V.unsafeWrite vec offset x
		writeList vec (offset + 1) xs
	
toList :: IOVector Word8 -> IO [Word8]
toList vec = if V.null vec then return [] else do
	h <- V.unsafeRead vec 0
	l <- toList $ V.tail vec
	return $ h : l

append :: TByteVector -> Text -> IO ()
append (TByteVector ic v) str = do
	vec <- atomically $ readTVar v
	nv <- do
		let veclen = V.length vec
		let bs = encodeUtf8 str
		let len = B.length bs
		nv <- V.unsafeGrow vec len
		let wordlist = B.unpack bs
		writeList nv veclen wordlist
		return nv
	atomically $ writeTVar v nv

read :: TByteVector -> Word -> Word -> STM ByteString
read (TByteVector ic v) offset len = do
	vec <- readTVar v
	let veclen = V.length vec
	let vecoffset = if offset > fromIntegral veclen then fromIntegral veclen else fromIntegral offset
	let slicelen = if vecoffset + fromIntegral len > veclen then veclen - vecoffset else fromIntegral len
	rv <- unsafeIOToSTM $ do
		wordlist <- toList $ V.slice vecoffset slicelen vec
		return $ B.pack wordlist
	return rv
