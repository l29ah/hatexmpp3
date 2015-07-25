{-# LANGUAGE OverloadedStrings #-}

module Log where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Map.Strict as MS
import Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Time
import Network.Xmpp.Internal hiding (priority, status)

import Config
import Types

putLog :: Jid -> Msg -> UTCTime -> Hate ()
putLog j m t = do
	s <- ask
	--liftIO $ print (j, m, t)
	liftIO $ TIO.putStr $ putTkabberLog t j "" m
	liftIO $ atomically $ do
		ls <- readTVar $ logs s
		case MS.lookup j ls of
			Nothing -> do
				logv <- newTVar $ [(t, m)]
				writeTVar (logs s) $ MS.insert j logv ls
			Just logv -> do
				log <- readTVar logv
				writeTVar logv $ (t, m) : log
	
getLastLogTS :: Jid -> Hate (Maybe UTCTime)
getLastLogTS j = do
	s <- ask
	ls <- readVar $ logs s
	runMaybeT $ do
		logv <- MaybeT $ return $ MS.lookup j ls
		log <- lift $ readVar logv
		pure $ fst $ head log

putTkabberLog :: UTCTime -> Jid -> Text -> Msg -> Text
putTkabberLog ts jid nick body = T.concat [
		"timestamp ",
		T.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%S" ts,
		" jid ",
		escape $ jidToText jid,
		" nick ",
		escape nick,
		" body ",
		escape body,
		" me 0\n"]
	where
		--escape t = if T.length t == 0 then "{}" else esc ' ' "\\\\ " $ esc '}' "\\}" $ esc '\n' "\\n" $ esc '\\' "\\\\" t
		escape t = if T.length t == 0 then "{}" else L.foldl (\it c -> esc c (T.pack $ '\\':'\\':c:[]) it) (esc '\n' "\\\\n" $ esc '\\' "\\\\\\\\" t) ("{}\" $[]" :: String)
		esc c e t = T.concat $ L.intersperse e $ T.split (== c) t
