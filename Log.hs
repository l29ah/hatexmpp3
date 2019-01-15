{-# LANGUAGE OverloadedStrings #-}

module Log where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Map.Strict as MS
import Data.Maybe
import Data.List as L
import Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Time
import Network.Xmpp.Internal hiding (priority, status)

import Config
import Types

getLog :: Jid -> Hate [LogEntry]
getLog jid = do
	s <- ask
	l <- liftIO $ readTVarIO $ logs s

	let resourceLogs =  L.filter (\j -> toBare j == jid) $ MS.keys l
	liftM (reverse . L.concat) $ liftIO $ atomically $ sequence $ L.map (\res -> case MS.lookup res l of
			Nothing -> return []
			Just logv ->
				readTVar $ fst logv
		) resourceLogs

getTChanContents :: TChan a -> IO [a]
getTChanContents c = do
	x <- atomically $ readTChan c
	cont <- getTChanContents c
	return $ x : cont

getLogLazy :: Jid -> Hate [LogEntry]
getLogLazy jid = do
	s <- ask
	l <- liftIO $ readTVarIO $ logs s
	liftIO $ maybe (return []) (\logv -> (\(list, tchan) -> liftM (list ++) $ getTChanContents tchan) =<< (atomically $ do
			past <- readTVar $ fst logv
			future <- dupTChan $ snd logv
			return (past, future))) $ MS.lookup jid l

showLog :: [LogEntry] -> Text
showLog = T.unlines . L.map (\(t, mn, m) -> T.concat
		[S.toText (show t)
		, "\t"
		, fromMaybe "" mn
		, "\t"
		, m])

getLogS j = (liftM showLog) $ getLog j
getLogLazyS j = (liftM showLog) $ getLogLazy j

putLog :: Jid -> Msg -> Maybe Nickname -> UTCTime -> Hate ()
putLog j m mn t = do
	s <- ask
	--liftIO $ print (j, m, t)
	liftIO $ TIO.putStr $ putTkabberLog $ TkabberLog t j "" m 0
	liftIO $ atomically $ do
		ls <- readTVar $ logs s
		case MS.lookup j ls of
			Nothing -> do
				logv <- newTVar $ [(t, mn, m)]
				logc <- newBroadcastTChan
				writeTChan logc (t, mn, m)
				writeTVar (logs s) $ MS.insert j (logv, logc) ls
			Just (logv, logc) -> do
				log <- readTVar logv
				writeTVar logv $ (t, mn, m) : log
				writeTChan logc (t, mn, m)
	
getLastLogTS :: Jid -> Hate (Maybe UTCTime)
getLastLogTS j = do
	s <- ask
	ls <- readVar $ logs s
	runMaybeT $ do
		logv <- MaybeT $ return $ MS.lookup j ls
		log <- lift $ readVar $ fst logv
		pure $ (\(time,_,_)->time) $ L.head log

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
