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
import Data.Default
import Data.IORef
import Data.List as L
import Data.Map as M
import Data.Map.Strict as MS
import Data.Maybe
import Data.String.Class as S
import Data.Text as T
import Data.Text.Encoding as E
import Data.Time
import Data.Word
import Data.XML.Types as DXT
import Network.NineP as NP
import Network.NineP.Error
import Network.NineP.File
import Network.TLS
import Network.Xmpp
import Network.Xmpp.Extras.AdHocCommands
import Network.Xmpp.Extras.IQAvatar
import Network.Xmpp.Extras.MUC
import Network.Xmpp.Extras.VCardAvatar
import Network.Xmpp.Internal hiding (priority, status)
import System.Environment
import System.Log.Logger
import qualified Text.XML as TX

import Config
import Log
import MUC
import Types

import Debug.Trace

dbg = debugM "HateXMPP"

catchXmpp :: Either XmppFailure Session -> IO Session
catchXmpp = either throw return

--chatFileRead :: Word64 -> Word32 -> Hate ByteString
chatFileRead typ jid offset len = getLogLazyS jid (fromIntegral offset) (fromIntegral len)

chatFileWrite typ jid text = do
	s <- ask
	se <- readVar $ sess s
	liftIO $ do
		result <- sendMessage ((simpleIM jid $ toText text) { messageType = typ }) se
		return $ either (throw . OtherError . show) (id) result

chatFile jid = --simpleFile (T.unpack $ jidToText jid)
	(rwFile (T.unpack $ jidToText jid) Nothing (Just $ chatFileWrite Chat jid)) { NP.read = chatFileRead Chat jid }

vcAvatarRead jid = do
	s <- ask
	se <- readVar $ sess s
	result <- liftIO $ askvCard jid se
	liftIO $ traceIO $ show result
	undefined
vcAvatarWrite jid = undefined

vcAvatarFile jid = rwFile "avatar" (Just $ vcAvatarRead jid) (Just $ vcAvatarWrite jid)

vcardDir jid = (boringDir "vcard" []) {
		getFiles = do
			return [avatarFile jid],
		descend = \name -> do
			--maybe (throw $ ENoFile name) (return . chatFile) $
				--jidFromText $ T.pack name
			return $ vcAvatarFile jid
	}

avatarRead jid = do
	s <- ask
	se <- readVar $ sess s
	result <- liftIO $ askIQAvatar jid se
	liftIO $ traceIO $ show result
	undefined
avatarWrite jid = undefined

avatarFile jid = rwFile "avatar" (Just $ avatarRead jid) (Just $ avatarWrite jid)

rosterItem jid = (boringDir (T.unpack $ jidToText jid) []) {
		getFiles = do
			return [chatFile jid, vcardDir jid, avatarFile jid],
		descend = \name -> do
			case name of
				"avatar" -> return $ avatarFile jid
				"vcard" -> return $ vcardDir jid
				_ -> maybe (throw $ ENoFile name) (return . chatFile) $
					jidFromText $ T.pack name
	}

rosterDir :: NineFile Hate
rosterDir = (boringDir "roster" []) {
		getFiles = do
			s <- ask
			se <- readVar $ sess s
			roster <- liftIO $ getRoster se
			liftIO $ Prelude.putStrLn $ "roster ver: " ++ (show $ ver roster)
			--liftIO $ print $ fmap (show . riJid) $ items roster
			return $ fmap (rosterItem) $ keys $ items roster,
		descend = \name -> do
			s <- ask
			se <- readVar $ sess s
			roster <- liftIO $ getRoster se
			maybe (throw $ ENoFile name) (return . rosterItem) $ do
				jid <- jidFromText $ T.pack name
				--M.lookup jid $ items roster
				return jid
	}

readMUCChat jid = chatFileRead GroupChat jid
writeMUCChat jid = chatFileWrite GroupChat jid

mucChat jid = (rwFile "__chat" Nothing (Just $ writeMUCChat jid)) { NP.read = chatFileRead Chat jid }

muc jid = boringDir (T.unpack $ jidToText jid) [("__chat", mucChat jid)]

mucsmkdir name = do
	se <- readVarH (readTVarIO . sess)
	nick <- readVarH (readTVarIO . muc_default_nick)
	let barejid = fromMaybe (throw EInval) $ jidFromText $ T.pack name
	let (localp, domainp, _) = jidToTexts barejid
	let jid = fromMaybe (throw EInval) $ jidFromTexts localp domainp (Just $ T.pack nick)
	-- TODO clear idea about how much of the history to request
	liftIO $ joinMUC jid (Just $ def { mhrSeconds = Just 200 }) se
	addMUC barejid nick
	--liftIO $ sendMessage ((simpleIM ((fromJust $ jidFromTexts (Just "hikkiecommune") "conference.bitcheese.net" Nothing)) "Voker57: i hate you") { messageType = GroupChat }) se
	--liftIO $ sendMUC (toBare jid) "i hate you" se
	--let jid = fromMaybe (throw EInval) $ jidFromTexts localp domainp (Just "dosmot")
	return $ muc barejid
	--throw $ ENotImplemented "mucmkdir"

mucsDir :: NineFile Hate
mucsDir = (boringDir "mucs" []) {
		getFiles = do
			s <- ask
			ms <- readVar $ mucs s
			return $ fmap (muc) $ keys ms,
		descend = \name -> do
			s <- ask
			ms <- readVar $ mucs s
			maybe (throw $ ENoFile name) (return . muc) $ do
				jid <- jidFromText $ T.pack name
				M.lookup jid ms
				return jid,
		create = \name perms -> if isDir perms then mucsmkdir name else throw EInval
	}

sendRaw :: B.ByteString -> Session -> IO (Either XmppFailure ())
sendRaw d s = semWrite (writeSemaphore s) d

processOtherFeatures :: Session -> DXT.Element -> Hate ()
processOtherFeatures s e = do
	s <- ask
	forM_ (TX.nameNamespace $ DXT.elementName e) $ \ns -> do
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
					body <- MaybeT $ pure $ L.find (\DXT.Element { DXT.elementName = TX.Name n ns pre } -> n == "body") pld
					let l = DXT.elementNodes body
					content <- MaybeT $ pure $ L.find (const True) l
					text <- case content of
						DXT.NodeContent (ContentText t) -> pure t
						_ -> mzero
					now <- liftIO $ getCurrentTime
					let delayed_ts = do
						delaye <- L.find (\DXT.Element { DXT.elementName = TX.Name n ns pre } -> n == "delay") pld
						stampa <- L.find (\(DXT.Name n ns pre, _) -> n == "stamp") $ DXT.elementAttributes delaye
						content <- L.find (const True) $ snd stampa
						t <- case content of
							ContentText t -> pure t
							_ -> mzero
						parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ T.unpack t :: Maybe UTCTime
					let timestamp = fromMaybe now delayed_ts
					let nick = if (typ == GroupChat)
						then resourcepart f
						else Nothing
					let saneFrom = if (typ == GroupChat)
						then toBare f
						else f
					lift $ putLog saneFrom text nick timestamp
			PresenceS p@(Presence id from to lang typ pld attr) -> do
				if L.null pld
					then liftIO $ dbg $ show ("simple presence", from, typ)
					else forM_ pld $ \e -> do
						let en = DXT.elementName e
						if TX.nameLocalName en == "x" && TX.nameNamespace en == Just "http://jabber.org/protocol/muc#user"
							then liftIO $ dbg $ show ("a muc guy has changed presence", from, typ)
							else liftIO $ dbg $ show ("unknown presence", from, typ, pld)
			_ -> liftIO $ dbg $ show stanza

connectS tsess = do
	s <- ask
	features <- liftIO $ getFeatures tsess
	mapM_ (processOtherFeatures tsess) $ streamFeaturesOther features
	-- Enable SM
	sme <- readVar $ streamManagement s
	when sme $ do
		smf <- readVar $ featureStreamManagement3 s
		when smf $ do
			liftIO $ sendRaw "<enable xmlns='urn:xmpp:sm:3'/>" tsess
			return ()
	liftIO $ initRoster tsess
	liftIO $ sendPresence def tsess
	writeVar (sess s) tsess
	liftIO $ forkIO $ receiver s tsess

streamManagementPlugin :: IO Plugin
streamManagementPlugin = do
	stanzaReceivedCount <- newIORef 0 :: IO (IORef Word32)
	return $ (\out -> return $ Plugin'
		{ inHandler = \sta as -> do
			modifyIORef' stanzaReceivedCount succ
			case sta of
				XmppNonza e -> do
					case TX.nameLocalName $ DXT.elementName e of
						"enabled" -> writeIORef stanzaReceivedCount 0
						"r" -> do
							h <- readIORef stanzaReceivedCount
							out $ XmppNonza $ DXT.Element "a" [
								("xmlns",	[DXT.ContentText "urn:xmpp:sm:3"]),
								("h",		[DXT.ContentText $ T.pack $ show h])
								] []
							return ()
						_ -> return ()
				_ -> return ()
			return [(sta, as)]
		, outHandler = out
		, onSessionUp = const $ return ()
		})

rootmkdir "roster" = do
		s <- ask
		serv <- readSVar $ server s
		user <- readSVar $ username s
		pass <- readSVar $ password s
		res <- readSVar $ resource s
		unsafeCerts <- readVar $ permitUnsafeCerts s
		sMP <- liftIO $ streamManagementPlugin
		tsess <- liftIO (catchXmpp =<< session serv
				(Just (\_ -> ([scramSha1 user Nothing pass]), if res == "" then Nothing else Just res))
				(def	{
						sessionStreamConfiguration = def {
							tlsBehaviour = RequireTls,
							tlsParams = if unsafeCerts
								then xmppDefaultParams { clientHooks = def { onServerCertificate = \_ _ _ _ -> pure [] } }
								else xmppDefaultParams
						},
						enableRoster = False,
						plugins = [sMP],
						onConnectionClosed = \sess _ -> do
							BLC.putStrLn "Disconnected. Reconnecting..."
							_ <- reconnect' sess
							flip runHate s $ do
								connectS sess
								rejoinMUCs
							return ()
					}))
		connectS tsess
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

	(rootdir, rootref) <- simpleDirectory "/" (throw $ EInval) rootmkdir
	writeIORef rootref [("config", configDir), ("mucs", mucsDir)]
	let ncfg = Config {
		root = rootdir,
		addr = a,
		monadState = state
	}
	return (state, run9PServer ncfg)

ghciMain = do
	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
	updateGlobalLogger "Network.NineP" $ setLevel DEBUG
	updateGlobalLogger "HateXMPP" $ setLevel DEBUG
	(state, runServer) <- initMain
	forkIO $ runServer
	return state

main = do
	(_, runServer) <- initMain
	runServer
