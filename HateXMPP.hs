{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts, CPP #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Default
import Data.IORef
import Data.List as L
import qualified Data.Map as M
import Data.Map.Strict (keys)
import qualified Data.Map.Strict as MS
import Data.Maybe
import Data.String.Class (toText)
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
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
import qualified Network.Xmpp.IM.Presence as IMP
import System.Console.GetOpt
import System.Environment
import System.Log.Logger
import qualified Text.XML as TX

import Config
import Log
import MUC
import Types
#ifdef UI_GTK
import GTK.GTK
import GTK.Chat
import qualified GTK.Roster as GR
#endif

import Debug.Trace

data Options = Options
	{ oXMPPLogPrio :: Priority
	, o9PLogPrio :: Priority
	} deriving (Eq, Show)

defaultOptions = Options
	{ oXMPPLogPrio = WARNING
	, o9PLogPrio = WARNING
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['d']	["debug-9p"]	(NoArg	(\o -> o { o9PLogPrio = DEBUG }))		"Debug 9P messages"
	, Option ['v']	["verbose"]	(NoArg	(\o -> o { oXMPPLogPrio = DEBUG }))		"Be verbose on what's happening on the XMPP wire"
	]

getOpts :: IO (Options, [String])
getOpts = do
	args <- getArgs
	pn <- getProgName
	case getOpt Permute options args of
		(o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ pn ++ " [options]") options))

dbg = debugM "HateXMPP"

catchXmpp :: Either XmppFailure Session -> IO Session
catchXmpp = either throw return

chatFileRead :: MessageType -> Jid -> Word64 -> Word32 -> Hate BLC.ByteString
chatFileRead typ jid offset len = getLogLazyS jid (fromIntegral offset) (fromIntegral len)

chatFileWrite :: MessageType -> Jid -> BL.ByteString -> Hate ()
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
			liftIO $ dbg $ "roster ver: " ++ (show $ ver roster)
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
	s <- ask
	se <- readVarH (readTVarIO . sess)
	nick <- readVarH (readTVarIO . muc_default_nick)
	let barejid = fromMaybe (throw EInval) $ jidFromText $ T.pack name
	let (localp, domainp, _) = jidToTexts barejid
	let jid = fromMaybe (throw EInval) $ jidFromTexts localp domainp (Just $ T.pack nick)
#ifdef UI_GTK
	-- TODO error reporting
	let sendMsg text = sendMessage ((simpleIM barejid text) { messageType = GroupChat }) se >> pure ()
	addChat barejid sendMsg
	add <- liftIO $ readTVarIO $ addMUCToRosterWindow s
	liftIO $ add barejid
#endif
	historyRequestSeconds <- readVarH (readTVarIO . muc_history_request)
	let historyRequest = if historyRequestSeconds < 0 then Nothing else Just $ def { mhrSeconds = Just historyRequestSeconds }
	liftIO $ joinMUC jid historyRequest se
	addMUC barejid nick
	return $ muc barejid

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

handleReceivedMessage from msg nick timestamp = do
	s <- ask
	chats <- liftIO $ readTVarIO $ chats s
	let entry = (timestamp, nick, msg)
	maybe (pure ()) (\handler -> liftIO $ handler entry) $ MS.lookup from $ chats
	putLog from msg nick timestamp

receiver :: GlobalState -> Session -> IO ()
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
					lift $ handleReceivedMessage saneFrom text nick timestamp
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
	liftIO $ forkIO $ updateStatus tsess (status s) (mucs s)
	writeVar (sess s) tsess
	liftIO $ forkIO $ receiver s tsess

updateStatus :: Session -> TVar String -> TVar MUCs -> IO ()
updateStatus tsess statusVar mucsVar = do
	initialStatus <- readTVarIO statusVar
	let sendStatus s = do
		let sendPresenceTo mjid = sendPresence (withIMPresence (def { IMP.status = Just $ toText s }) def { presenceTo = mjid }) tsess
		sendPresenceTo Nothing
		-- also send the presence update to all the MUCs we participate in
		currentMUCs <- readTVarIO mucsVar
		mapM_ (sendPresenceTo . Just) $ keys currentMUCs
	void $ sendStatus initialStatus
	fix (\again previousStatus -> do
		changedStatus <- atomically $ do
			newStatus <- readTVar statusVar
			check (newStatus /= previousStatus)
			pure newStatus
		void $ sendStatus changedStatus
		again changedStatus) initialStatus

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

rosterHandler :: Roster -> RosterUpdate -> IO ()
rosterHandler roster update = do
	print "got roster"
	print roster
	print update

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
						plugins = [sMP],
						onRosterPush = Just rosterHandler,
						onConnectionClosed = \sess why -> do
							noticeM "HateXMPP" $ "Disconnected (" ++ show why ++ "). Reconnecting..."
							_ <- reconnect' sess
							flip runHate s $ do
								connectS sess
								rejoinMUCs
							return ()
					}))
#ifdef UI_GTK
		GR.spawnRosterWindow
#endif
		connectS tsess
#ifdef UI_GTK
		-- submit the initial roster data to GUI
		add <- liftIO $ readTVarIO $ addUserToRosterWindow s
		roster <- liftIO $ getRoster tsess
		liftIO $ mapM_ add $ M.keys $ items roster
#endif
		return rosterDir

rootmkdir "test" = do
		s <- ask
		se <- readVar $ sess s
		liftIO $ Prelude.print =<< getRoster se
		liftIO $ Prelude.print "loh"
		throw $ ENotImplemented "test"
rootmkdir _ = throw $ EInval

initMain = do
	(opts, _) <- getOpts
	updateGlobalLogger "Pontarius.Xmpp" $ setLevel $ oXMPPLogPrio opts
	updateGlobalLogger "Network.NineP" $ setLevel $ o9PLogPrio opts
	updateGlobalLogger "HateXMPP" $ setLevel $ oXMPPLogPrio opts

	a <- getEnv "HATEXMPP_ADDRESS"
	state <- initState
#ifdef UI_GTK
	initGTK
#endif

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
