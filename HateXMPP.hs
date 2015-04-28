{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Error
import Control.Monad.Reader
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC
import Data.String.Class as S
import Data.Text
import Data.Text.Encoding as E
import Network.NineP
import Network.NineP.Error
import Network.NineP.File
import Network.Xmpp
import Network.Xmpp.Internal hiding (priority, status)
import System.Environment
import System.Log.Logger

type Hate = ReaderT GlobalState IO

runHate x s = runReaderT x s

instance EmbedIO Hate where
	type Content Hate = GlobalState
	embed f = do
		s <- ask
		liftIO $ f s
	callback action cont = runHate action cont

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

		sess :: TVar Session
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
				(configTVarReadT password)
				(Just (configTVarWriteT password)),
		rwf "password"
				(configTVarReadT password)
				(Just (configTVarWriteT password)),
		rwf "status"
				(configTVarRead status)
				(Just (configTVarWrite status))
	]

trimLn :: (Stringy s, Eq (StringCellChar s)) => s -> s
trimLn s = maybe "" (\c -> if c == (S.fromChar '\n') then S.init s else s) $ S.safeLast s
readVar :: TVar a -> ErrorT NineError Hate a
readVar = liftIO . atomically . readTVar
readSVar :: (Stringy a, Eq (StringCellChar a)) => TVar a -> ErrorT NineError Hate a
readSVar = liftM trimLn . readVar
writeVar v = liftIO . atomically . writeTVar v

main = do
	a <- getEnv "HATEXMPP_ADDRESS"
	st <- newTVarIO ""
	ut <- newTVarIO ""
	pt <- newTVarIO ""
	priot <- newTVarIO ""
	portt <- newTVarIO ""
	rt <- newTVarIO ""
	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
	let ncfg = Config {
		root = (boringDir "/" [
				("config", configDir)
			]) {
				create = \name perms ->
					if isDir perms
					then if name == "roster"
						then do
							s <- ask
							serv <- readSVar $ server s
							user <- readSVar $ username s
							pass <- readSVar $ password s
							res <- readSVar $ resource s
							tsess <- liftIO (catchXmpp =<< session serv
									(Just (\_ -> ([scramSha1 user Nothing pass]), if res == "" then Nothing else Just res))
									(def { sessionStreamConfiguration = def { tlsBehaviour = RequireTls } }))
							liftIO $ B.putStrLn "fuck you"
							liftIO $ sendPresence def tsess
							writeVar (sess s) tsess
							throwError $ ENotImplemented "roster"
						else throwError $ EPermissionDenied
					else throwError $ EPermissionDenied
			},
		addr = a,
		monadState = GlobalState {
				server = st,
				username = ut,
				password = pt,
				priority = priot,
				port = portt,
				resource = rt
			}
	}
--	runReaderT (run9PServer ncfg) (GlobalState {
--			server = st,
--			username = ut,
--			password = pt
--		})
	run9PServer ncfg
