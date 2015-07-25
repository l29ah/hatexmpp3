{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.String.Class as S
import Data.Text as T
import Data.Text.Encoding as E
import Data.Time
import Data.XML.Types
import Network.NineP
import Network.NineP.Error
import Network.NineP.File
import Network.TLS
import Network.Xmpp
import Network.Xmpp.Internal hiding (priority, status)
import System.Environment
import System.Log.Logger

import Config
import Log
import Types

catchXmpp :: Either XmppFailure Session -> IO Session
catchXmpp = either throw return

rosterItem jid = boringFile jid

rosterDir :: NineFile Hate
rosterDir = (boringDir "roster" []) {
		getFiles = do
			s <- ask
			se <- readVar $ sess s
			roster <- liftIO $ getRoster se
			return []
	}

readMUCChat jid = undefined
writeMUCChat jid = do
	--se <- readVarH (readTVarIO . sess)
	--liftIO $ sendMessage ((simpleIM ((fromJust $ jidFromTexts (Just "hikkiecommune") "conference.bitcheese.net" Nothing)) "i hate you") { messageType = GroupChat }) se
	undefined
	

mucChat jid = rwFile jid (Just $ readMUCChat jid) (Just $ writeMUCChat jid)

muc jid = boringDir jid [("__chat", mucChat jid)]

mucsmkdir name = do
	se <- readVarH (readTVarIO . sess)
	nick <- readVarH (readTVarIO . muc_default_nick)
	let barejid = fromMaybe (throw EInval) $ jidFromText $ T.pack name
	let (localp, domainp, _) = jidToTexts barejid
	let jid = fromMaybe (throw EInval) $ jidFromTexts localp domainp (Just $ T.pack nick)
	-- TODO clear idea about how much of the history to request
	liftIO $ sendPresence ((presTo presence jid) { presencePayload = [Element "x" [("xmlns", [ContentText "http://jabber.org/protocol/muc"])] [NodeElement $ Element "history" [("seconds", [ContentText "200"])] []]] } ) se
	--liftIO $ sendMessage ((simpleIM ((fromJust $ jidFromTexts (Just "hikkiecommune") "conference.bitcheese.net" Nothing)) "Voker57: i hate you") { messageType = GroupChat }) se
	--liftIO $ sendMessage ((simpleIM ((fromJust $ jidFromTexts (Just "hikkiecommune") "conference.bitcheese.net" Nothing)) "i hate you") { messageType = GroupChat }) se
	return $ muc name
	--throw $ ENotImplemented "mucmkdir"

mucsDir :: NineFile Hate
mucsDir = (boringDir "mucs" []) {
		getFiles = do
			liftIO $ Prelude.putStrLn "lolz"
			return [],
		create = \name perms -> if isDir perms then mucsmkdir name else throw EInval
	}

sendRaw :: B.ByteString -> Session -> IO (Either XmppFailure ())
sendRaw d s = semWrite (writeSemaphore s) d

processOtherFeatures :: Session -> Element -> Hate ()
processOtherFeatures s e = do
	s <- ask
	forM_ (nameNamespace $ elementName e) $ \ns -> do
		case ns of
			"urn:xmpp:sm:3" -> do
				-- For client-to-server connections, the client MUST NOT attempt to enable stream management until after it has completed Resource Binding unless it is resuming a previous session (see Resumption).
				writeVar (featureStreamManagement3 s) True
			_ -> return ()

receiver s se = flip runHate s $ forever $ do
		(stanza, ann) <- liftIO $ getStanza se
		case stanza of
			MessageS (Message id from to lang typ pld attr) -> void $ runMaybeT $ do
					f <- MaybeT $ pure from
					body <- MaybeT $ pure $ L.find (\Element { elementName = Name n ns pre } -> n == "body") pld
					let l = elementNodes body
					content <- MaybeT $ pure $ L.find (const True) l
					text <- case content of
						NodeContent (ContentText t) -> pure t
						_ -> mzero
					now <- liftIO $ getCurrentTime
					let delayed_ts = do
						delaye <- L.find (\Element { elementName = Name n ns pre } -> n == "delay") pld
						stampa <- L.find (\(Name n ns pre, _) -> n == "stamp") $ elementAttributes delaye
						content <- L.find (const True) $ snd stampa
						t <- case content of
							ContentText t -> pure t
							_ -> mzero
						parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ T.unpack t :: Maybe UTCTime
					let timestamp = fromMaybe now delayed_ts
					lift $ putLog f text timestamp
			_ -> pure ()

rootmkdir "roster" = do
		s <- ask
		serv <- readSVar $ server s
		user <- readSVar $ username s
		pass <- readSVar $ password s
		res <- readSVar $ resource s
		unsafeCerts <- readVar $ permitUnsafeCerts s
		tsess <- liftIO (catchXmpp =<< session serv
				(Just (\_ -> ([scramSha1 user Nothing pass]), if res == "" then Nothing else Just res))
				(def {	sessionStreamConfiguration = def {
						tlsBehaviour = RequireTls,
						tlsParams = if unsafeCerts
							then xmppDefaultParams { clientHooks = def { onServerCertificate = \_ _ _ _ -> pure [] } }
							else xmppDefaultParams,
						--establishSession = False }, -- to be able to use stream management
						establishSession = True },
					enableRoster = False }))
		features <- liftIO $ getFeatures tsess
		mapM_ (processOtherFeatures tsess) $ streamOtherFeatures features
		-- Enable SM
		sme <- readVar $ streamManagement s
		when sme $ do
			smf <- readVar $ featureStreamManagement3 s
			when smf $ do
				liftIO $ sendRaw "<enable xmlns='urn:xmpp:sm:3'/>" tsess
				-- TODO error reporting
				--liftIO $ flip withConnection tsess $ \stream -> do
					--e <- withStream pullElement stream
					--print e
					--return ((), stream)
				liftIO $ flip withConnection tsess $ \stream -> return ((), stream)
				--liftIO $ Prelude.putStrLn "SM!"
				return ()
			-- TODO resume session
		-- TODO startSession
		-- TODO initRoster
		liftIO $ sendPresence def tsess
		writeVar (sess s) tsess
		--liftIO $ Prelude.print =<< getRoster tsess
		liftIO $ forkIO $ receiver s tsess
		return rosterDir
rootmkdir "test" = do
		s <- ask
		se <- readVar $ sess s
		liftIO $ Prelude.print =<< getRoster se
		liftIO $ Prelude.print "loh"
		throw $ ENotImplemented "test"
rootmkdir _ = throw $ EInval

initMain = do
	a <- getEnv "HATEXMPP_ADDRESS"
	state <- initState

	--updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	(rootdir, rootref) <- simpleDirectory "/" (throw $ EInval) rootmkdir
	writeIORef rootref [("config", configDir), ("mucs", mucsDir)]
	let ncfg = Config {
		root = rootdir,
		addr = a,
		monadState = state
	}
	return (state, run9PServer ncfg)

ghciMain = do
	(state, runServer) <- initMain
	forkIO $ runServer
	return state

main = do
	(_, runServer) <- initMain
	runServer
