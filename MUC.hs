{-# LANGUAGE OverloadedStrings #-}

module MUC where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Data.Default
import qualified Data.Map.Strict as MS
import Data.Maybe
import qualified Data.Text as T
import Network.Xmpp.Extras.MUC
import Network.Xmpp.Internal hiding (priority, status)

import Config
import Log
import Types

addMUC :: Jid -> String -> Hate ()
addMUC jid nick = do
	initLog jid
	s <- ask
	liftIO $ atomically $ do
		ms <- readTVar $ mucs s
		writeTVar (mucs s) $ MS.insert jid (MUC nick) ms

rejoinMUCs :: Hate ()
rejoinMUCs = do
	ms <- readVarH (readTVarIO . mucs)
	se <- readVarH (readTVarIO . sess)
	liftIO $ sequence_ $ map (\(barejid, (MUC nick)) -> do
			let (localp, domainp, _) = jidToTexts barejid
			let jid = (\x -> assert (isJust x) (fromJust x)) $ jidFromTexts localp domainp (Just $ T.pack nick)
			joinMUC jid (Just $ def { mhrSeconds = Just 200 }) se
			return ()
		) $ MS.toList ms
	-- TODO clear idea about how much of the history to request
