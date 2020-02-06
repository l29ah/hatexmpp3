{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Log
	( getLogLazyS
	, putLog
	) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Map.Strict as MS
import Data.Maybe
import Data.List as L
import Data.Sequence as Seq
import Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Time
import Network.Xmpp.Internal hiding (priority, status)

import Config
import TByteVector as TBV
import Types

lookupLog :: Jid -> Hate (Maybe Log)
lookupLog jid = do
	s <- ask
	l <- liftIO $ readTVarIO $ logs s
	return $ MS.lookup jid l

showLog :: [LogEntry] -> Text
showLog = T.unlines . L.map (\(t, mn, m) -> T.concat
		[S.toText (show t)
		, "\t"
		, fromMaybe "" mn
		, "\t"
		, m])

fillLogS :: Log -> IO ()
fillLogS (Log tentries tunshown shown) = do
	(newLogEntry, unshown) <- atomically $ do
		entries <- readTVar tentries
		unshown <- readTVar tunshown
		maybe retry (\ent -> return (ent, unshown)) $ Seq.lookup unshown entries
	TBV.append shown $ showLog [newLogEntry]
	atomically $ writeTVar tunshown (unshown + 1)

readLogLazyS :: Log -> Word -> Word -> IO BL.ByteString
readLogLazyS log offset len = do
	cached <- atomically $ TBV.read (shownLog log) offset len
	if B.null cached
	then do
		fillLogS log
		readLogLazyS log offset len
	else return $ BL.fromStrict cached

getLogLazyS :: Jid -> Word -> Word -> Hate BL.ByteString
getLogLazyS jid offset len = do
	log <- lookupLog jid
	liftIO $ maybe (return BL.empty) (\l -> readLogLazyS l offset len) log

newLog :: IO Log
newLog = do
	le <- newTVarIO Seq.empty
	idx <- newTVarIO 0
	sl <- newTByteVector
	return $ Log le idx sl

putLog :: Jid -> Msg -> Maybe Nickname -> UTCTime -> Hate ()
putLog j m mn t = do
	s <- ask
	--liftIO $ print (j, m, t)
	--liftIO $ TIO.putStr $ putTkabberLog $ TkabberLog t j "" m 0
	newlog <- liftIO newLog
	liftIO $ atomically $ do
		ls <- readTVar $ logs s
		case MS.lookup j ls of
			Nothing -> do
				writeTVar (logEntries newlog) $ Seq.singleton (t, mn, m)
				writeTVar (logs s) $ MS.insert j newlog ls
			Just (Log entries _ _) -> do
				modifyTVar entries $ \seq -> seq |> (t, mn, m)

data TkabberLog = TkabberLog {
		timestamp :: UTCTime,
		jid :: Jid,
		nick :: Text,
		body :: Msg,
		me :: Int
	}

--getTkabberLog :: Text -> TkabberLog

putTkabberLog :: TkabberLog -> Text
putTkabberLog (TkabberLog ts jid nick body me) = T.concat [
		"timestamp ",
		T.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" ts,
		" jid ",
		escape $ jidToText jid,
		" nick ",
		escape nick,
		" body ",
		escape body,
		" me ",
		T.pack $ show me,
		"\n"]
	where
		--escape t = if T.length t == 0 then "{}" else esc ' ' "\\\\ " $ esc '}' "\\}" $ esc '\n' "\\n" $ esc '\\' "\\\\" t
		escape t = if T.length t == 0 then "{}" else L.foldl (\it c -> esc c (T.pack $ '\\':'\\':c:[]) it) (esc '\n' "\\\\n" $ esc '\\' "\\\\\\\\" t) ("{}\" $[]" :: String)
		esc c e t = T.concat $ L.intersperse e $ T.split (== c) t
