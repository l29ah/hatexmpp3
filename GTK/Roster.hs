{-# LANGUAGE OverloadedStrings #-}
module GTK.Roster
	( spawnRosterWindow
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Graphics.UI.Gtk hiding (eventKeyName, eventModifier)
import qualified Graphics.UI.Gtk.ModelView as MView
import Network.Xmpp as X

import Types
import GTK.Chat

treeMUCIndex = 0
treeUserIndex = 1

data RosterTreeNode = MUCs | Users | JID X.MessageType Jid deriving (Eq, Show)

defaultTree :: Forest RosterTreeNode
defaultTree = [Node MUCs [], Node Users []]

renderNode :: RosterTreeNode -> Text
renderNode MUCs = "MUCs"
renderNode Users = "Users"
renderNode (JID _ jid) = jidToText jid

spawnRosterWindow :: (X.MessageType -> Jid -> Text -> IO ()) -> Hate ()
spawnRosterWindow sendMessage = do
	s <- ask
	addToRoster <- liftIO $ postGUISync $ do
		w <- windowNew
		set w [windowTitle := ("hatexmpp roster" :: Text)]
		windowSetDefaultSize w 300 800

		view <- MView.treeViewNew
		treeViewSetHeadersVisible view False
		store <- MView.treeStoreNew defaultTree
		MView.treeViewSetModel view $ Just store

		column <- MView.treeViewColumnNew
		MView.treeViewAppendColumn view column
		MView.treeViewColumnSetTitle column T.empty
		cell <- cellRendererTextNew
		MView.treeViewColumnPackStart column cell True
		cellLayoutSetAttributes column cell store (\record -> [MView.cellText := renderNode record])

		on view rowActivated $ \path _ -> do
			val <- treeStoreGetValue store path
			case val of
				JID typ jid -> do
					forkIO $ flip runHate s $ addChat jid (sendMessage typ jid)
					pure ()
				_ -> pure ()

		scroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
		containerAdd scroll view

		containerAdd w scroll
		widgetShowAll w
		pure (\index typ jid -> postGUISync $ do
			let path = [index]
			pathIter <- treeModelGetIter store path
			unusedIndex <- treeModelIterNChildren store pathIter
			treeStoreInsert store path unusedIndex $ JID typ jid)
	let addMUCToRoster = addToRoster treeMUCIndex GroupChat
	let addUserToRoster = addToRoster treeUserIndex Chat
	liftIO $ atomically $ do
		writeTVar (addMUCToRosterWindow s) addMUCToRoster
		writeTVar (addUserToRosterWindow s) addUserToRoster
