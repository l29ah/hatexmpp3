{-# LANGUAGE OverloadedStrings #-}
module GTK.Chat
	( addChat
	) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Reader
import Data.DateTime
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.String.Class as S
import Data.Text as T
import Data.Text.IO as TIO
import Data.Time
import Graphics.UI.Gtk hiding (eventKeyName, eventModifier)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Abstract.Paned
import Network.Xmpp
import System.IO

import Types

renderMessage :: (UTCTime, Maybe Nickname, Msg) -> Text
renderMessage (t, mn, m) = T.concat
		[ S.fromString $ formatDateTime "[%T]" t
		, "<"
		, fromMaybe "" mn
		, ">"
		, m
		, "\n"]

addChat :: Jid -> (Msg -> IO ()) -> Hate ()
addChat jid message_cb = do
	handler <- liftIO $ postGUISync $ do
		w <- windowNew
		windowSetTitle w ("glovexmpp" :: Text)
		windowSetDefaultSize w 500 500

		logb <- textBufferNew Nothing
		logv <- textViewNewWithBuffer logb
		textViewSetEditable logv False
		textViewSetCursorVisible logv False

		inputb <- textBufferNew Nothing
		inputv <- textViewNewWithBuffer inputb
		textViewSetAcceptsTab inputv True
		onKeyPress inputv $ inputKeyPressed inputb message_cb
		panels <- vPanedNew
		panedPack1 panels logv True False
		panedPack2 panels inputv True False

		set w [ containerChild := panels ]
		widgetShowAll w
		Rectangle _ _ _ height <- widgetGetAllocation panels
		panedSetPosition panels (round (0.9 * fromIntegral height))
	
		pure (\entry -> do
			postGUISync $ bufferAdd logb $ renderMessage entry
			ei <- postGUISync $ textBufferGetEndIter logb
			postGUISync $ textViewScrollToIter logv ei 0 Nothing
			pure ()
			)
	s <- ask
	liftIO $ atomically $ modifyTVar (chats s) $ M.insert jid handler

bufferGet :: TextBuffer -> IO Text
bufferGet tb = do
	si <- textBufferGetStartIter tb
	ei <- textBufferGetEndIter tb
	textBufferGetText tb si ei True

bufferAdd :: TextBuffer -> Text -> IO ()
bufferAdd tb s = do
	t <- bufferGet tb
	textBufferSetText tb $ T.concat [t, s]

-- Keypress event handler
inputKeyPressed inputb message_cb e = if (eventKeyName e == "Return") && (notElem Shift $ eventModifier e)
	then do	
		t <- bufferGet inputb
		TIO.putStrLn t
		message_cb t
		textBufferSetText inputb ("" :: Text)
		return True
	else return False
