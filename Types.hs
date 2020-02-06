{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Types where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Data.Map.Strict as MS
import Data.Sequence
import Data.Text as Text
import Data.Time.Clock
import Network.Xmpp

import TByteVector

type Hate = ReaderT GlobalState IO
type Nickname = Text
type Msg = Text
type LogEntry = (UTCTime, Maybe Nickname, Msg)
type Logs = MS.Map Jid Log
type MUCs = MS.Map Jid MUC

data Log = Log
	{ logEntries :: TVar (Seq LogEntry)
	, firstUnshownLogIdx :: TVar Int
	, shownLog :: TByteVector
	}

runHate = runReaderT

instance EmbedIO Hate where
	type Content Hate = GlobalState
	embed f = ask >>= liftIO . f
	callback = runHate

data ShowSt = SNone | SAway | SChat | SDND | SXA deriving Show

data MUC = MUC
	{ nick :: String
	} deriving Show

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
		permitUnsafeCerts :: TVar Bool,

		sess :: TVar Session,
		featureStreamManagement3 :: TVar Bool,
		logs :: TVar Logs,
		mucs :: TVar MUCs
	}
