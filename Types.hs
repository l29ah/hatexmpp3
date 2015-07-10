{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

module Types where

import Control.Concurrent.STM.TVar
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Data.Text as Text
import Network.Xmpp

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
		permitUnsafeCerts :: TVar Bool,

		sess :: TVar Session,
		featureStreamManagement3 :: TVar Bool
	}
