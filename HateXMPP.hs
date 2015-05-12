{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC
import Data.IORef
import Data.Maybe
import Data.String.Class as S
import Data.Text as Text
import Data.Text.Encoding as E
import Data.XML.Types
import Network.NineP
import Network.NineP.Error
import Network.NineP.File
import Network.Xmpp
import Network.Xmpp.Internal hiding (priority, status)
import System.Environment
import System.Log.Logger

type Hate = ReaderT GlobalState IO

runHate = runReaderT

instance EmbedIO Hate where
	type Content Hate = GlobalState
	embed f = ask >>= liftIO . f
	callback = runHate

data ShowSt = SNone | SAway | SChat | SDND | SXA

data GlobalState = GlobalState {
		server :: TVar String,
		port :: TVar String,
		username :: TVar Text,
		password :: TVar Text,
		resource :: TVar Text,
		priority :: TVar String,
		jiv_name :: TVar String,
		jiv_os :: TVar String,
		jiv_version :: TVar String,
		muc_default_nick :: TVar String,
		showst :: TVar ShowSt,
		status :: TVar String,
		streamManagement :: TVar Bool,

		sess :: TVar Session,
		featureStreamManagement3 :: TVar Bool
	}

catchXmpp :: Either XmppFailure Session -> IO Session
catchXmpp = either throw return

--makeConfigFile :: String -> NineFile Hate
--makeConfigFile filename handler = undefined

configTVarRead acc = Just (liftM BLC.pack . liftIO . atomically . readTVar . acc =<< ask)
configTVarWrite acc x = liftIO . atomically . flip writeTVar (BLC.unpack x) . acc =<< ask
configTVarReadT acc = Just (liftM (fromChunks . (:[]) . E.encodeUtf8) . liftIO . atomically . readTVar . acc =<< ask)
configTVarWriteT acc x = liftIO . atomically . flip writeTVar (E.decodeUtf8 $ B.concat $ toChunks $ x) . acc =<< ask

rwf n a b = (n, rwFile n a b)

configDir :: NineFile Hate
configDir = boringDir "config" [
		rwf "server"
				(configTVarRead server)
				(Just (configTVarWrite server)),
		rwf "username"
				(configTVarReadT username)
				(Just (configTVarWriteT username)),
		rwf "password"
				(configTVarReadT password)
				(Just (configTVarWriteT password)),
		rwf "status"
				(configTVarRead status)
				(Just (configTVarWrite status))
	]

trimLn :: (Stringy s, Eq (StringCellChar s)) => s -> s
trimLn s = maybe "" (\c -> if c == (S.fromChar '\n') then S.init s else s) $ S.safeLast s
readVar :: TVar a -> Hate a
readVar = liftIO . atomically . readTVar
readVarH :: (GlobalState -> IO a) -> Hate a
readVarH acc = do
	s <- ask
	liftIO $ acc s
readSVar :: (Stringy a, Eq (StringCellChar a)) => TVar a -> Hate a
readSVar = liftM trimLn . readVar
writeVar v = liftIO . atomically . writeTVar v

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
	undefined
	

mucChat jid = rwFile jid (Just $ readMUCChat jid) (Just $ writeMUCChat jid)

muc jid = boringDir jid [("__chat", mucChat jid)]

mucsmkdir name = do
	se <- readVarH (readTVarIO . sess)
	nick <- readVarH (readTVarIO . muc_default_nick)
	let barejid = fromMaybe (throw EInval) $ jidFromText $ Text.pack name
	let (localp, domainp, _) = jidToTexts barejid
	let jid = fromMaybe (throw EInval) $ jidFromTexts localp domainp (Just $ Text.pack nick)
	liftIO $ sendPresence (presTo presence jid) se
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

rootmkdir "roster" = do
		s <- ask
		serv <- readSVar $ server s
		user <- readSVar $ username s
		pass <- readSVar $ password s
		res <- readSVar $ resource s
		tsess <- liftIO (catchXmpp =<< session serv
				(Just (\_ -> ([scramSha1 user Nothing pass]), if res == "" then Nothing else Just res))
				(def {	sessionStreamConfiguration = def {
						tlsBehaviour = RequireTls,
						establishSession = False }, -- to be able to use stream management
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
		liftIO $ Prelude.print =<< getRoster tsess
		return rosterDir
rootmkdir "test" = do
		s <- ask
		se <- readVar $ sess s
		liftIO $ Prelude.print =<< getRoster se
		liftIO $ Prelude.print "loh"
		throw $ ENotImplemented "test"
rootmkdir _ = throw $ EInval

initState = do
	st <- newTVarIO ""
	ut <- newTVarIO ""
	pt <- newTVarIO ""
	priot <- newTVarIO ""
	portt <- newTVarIO ""
	rt <- newTVarIO ""
	jnt <- newTVarIO "hatexmpp3"
	jot <- newTVarIO ""
	jvt <- newTVarIO ""
	mdnt <- newTVarIO "hatexmpp3"
	showt <- newTVarIO SNone
	statust <- newTVarIO ""
	streamManagementt <- newTVarIO False
	sesst <- newTVarIO undefined
	featureStreamManagement3t <- newTVarIO False

	return $ GlobalState {
				server = st,
				username = ut,
				password = pt,
				priority = priot,
				port = portt,
				resource = rt,
				jiv_name = jnt,
				jiv_os = jot,
				jiv_version = jvt,
				muc_default_nick = mdnt,
				showst = showt,
				status = statust,
				streamManagement = streamManagementt,
				sess = sesst,
				featureStreamManagement3 = featureStreamManagement3t
			}

initMain = do
	a <- getEnv "HATEXMPP_ADDRESS"
	state <- initState

	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

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
