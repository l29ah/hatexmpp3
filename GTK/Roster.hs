{-# LANGUAGE OverloadedStrings #-}
module GTK.Roster
	( spawnRosterWindow
	) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Graphics.UI.Gtk hiding (eventKeyName, eventModifier)
import qualified Graphics.UI.Gtk.ModelView as MView
import Network.Xmpp

import Types

treeMUCIndex = 0
treeUserIndex = 1

defaultTree :: Forest Text
defaultTree = [Node "MUCs" [], Node "Users" []]

spawnRosterWindow :: Hate ()
spawnRosterWindow = do
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
		cellLayoutSetAttributes column cell store (\record -> [MView.cellText := record])

		scroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
		containerAdd scroll view

		containerAdd w scroll
		widgetShowAll w
		pure (\index jid -> postGUISync $ do
			let path = [index]
			pathIter <- treeModelGetIter store path
			unusedIndex <- treeModelIterNChildren store pathIter
			treeStoreInsert store path unusedIndex $ jidToText jid)
	let addMUCToRoster = addToRoster treeMUCIndex
	let addUserToRoster = addToRoster treeUserIndex
	s <- ask
	liftIO $ atomically $ do
		writeTVar (addMUCToRosterWindow s) addMUCToRoster
		writeTVar (addUserToRosterWindow s) addUserToRoster
