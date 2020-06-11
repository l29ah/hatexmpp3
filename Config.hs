{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction, CPP #-}

module Config where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.ByteString as B
import Data.ByteString.Lazy.Char8 as BLC
import Data.Map.Strict as MS
import Data.String.Class as S
import Data.Text as Text
import Data.Text.Encoding as E
import Network.NineP
import Network.NineP.File

import Types

class Configable a where
	readcfg :: BLC.ByteString -> a
	showcfg :: a -> BLC.ByteString

instance Configable String where
	readcfg = BLC.unpack
	showcfg = BLC.pack
instance Configable Text where
	readcfg = E.decodeUtf8 . B.concat . toChunks
	showcfg = fromChunks . (:[]) . E.encodeUtf8
instance Configable Bool where
	readcfg = Prelude.read . BLC.unpack	-- TODO
	showcfg = BLC.pack . show
instance Configable Integer where
	readcfg = Prelude.read . BLC.unpack
	showcfg = BLC.pack . show

configTVarReadWith c acc = Just (liftM c . liftIO . readTVarIO . acc =<< ask)
configTVarWriteWith c acc x = liftIO . atomically . flip writeTVar (c x) . acc =<< ask
configTVarReadC = configTVarReadWith showcfg
configTVarWriteC = configTVarWriteWith readcfg

rwf n a b = (n, rwFile n a b)
rwfc n a = rwf n (configTVarReadC a) (Just (configTVarWriteC a))

configDir :: NineFile Hate
configDir = boringDir "config" [
		rwfc "server" server,
		rwfc "username" username,
		rwfc "password" password,
		rwfc "resource" resource,
		rwfc "muc_default_nick" muc_default_nick,
		rwfc "muc_history_request" muc_history_request,
		rwfc "status" status,
		rwfc "stream_management" streamManagement,
		rwfc "permit_all_certs" permitUnsafeCerts
	]

trimLn :: (Stringy s, Eq (StringCellChar s)) => s -> s
trimLn s = maybe "" (\c -> if c == (S.fromChar '\n') then S.init s else s) $ S.safeLast s
readVar :: TVar a -> Hate a
readVar = liftIO . readTVarIO
readVarH :: (GlobalState -> IO a) -> Hate a
readVarH acc = do
	s <- ask
	liftIO $ acc s
readSVar :: (Stringy a, Eq (StringCellChar a)) => TVar a -> Hate a
readSVar = liftM trimLn . readVar
writeVar v = liftIO . atomically . writeTVar v

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
	permitUnsafeCertst <- newTVarIO False
	mhrst <- newTVarIO (-1)
	sesst <- newTVarIO undefined
	featureStreamManagement3t <- newTVarIO False
	logst <- newTVarIO MS.empty
	mucst <- newTVarIO MS.empty
	chatst <- newTVarIO MS.empty
#ifdef UI_GTK
	amt <- newTVarIO undefined
	aut <- newTVarIO undefined
#endif

	return $ GlobalState
		{ server = st
		, username = ut
		, password = pt
		, priority = priot
		, port = portt
		, resource = rt
		, jiv_name = jnt
		, jiv_os = jot
		, jiv_version = jvt
		, muc_default_nick = mdnt
		, showst = showt
		, status = statust
		, streamManagement = streamManagementt
		, permitUnsafeCerts = permitUnsafeCertst
		, muc_history_request = mhrst
		, sess = sesst
		, featureStreamManagement3 = featureStreamManagement3t
		, logs = logst
		, mucs = mucst
		, chats = chatst
		, addMUCToRosterWindow = amt
		, addUserToRosterWindow = aut
		}
