{-# LANGUAGE OverloadedStrings #-}
module GTK.Chat
	( addChat
	) where

import Control.Concurrent.STM
import Control.Monad.Loops
import Control.Monad.Reader
import Data.DateTime
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.String.Class as S
import Data.Text as T
import Data.Time
import Graphics.UI.Gtk
import Network.Xmpp

import Types

renderMessage :: (UTCTime, Maybe Nickname, Msg) -> Text
renderMessage (t, mn, m) = T.concat
		[ "\n"
		, S.fromString $ formatDateTime "[%T]" t
		, "<"
		, fromMaybe "" mn
		, ">"
		, m
		]

addChat :: Jid -> (Msg -> IO ()) -> Hate ()
addChat jid message_cb = do
	handler <- liftIO $ postGUISync $ do
		w <- windowNew
		set w [windowTitle := jidToText jid]
		windowSetDefaultSize w 500 500

		logb <- textBufferNew Nothing
		logv <- textViewNewWithBuffer logb
		textViewSetWrapMode logv WrapWordChar
		textViewSetEditable logv False
		textViewSetCursorVisible logv False

		inputb <- textBufferNew Nothing
		inputv <- textViewNewWithBuffer inputb
		textViewSetAcceptsTab inputv True
		inputv `on` keyPressEvent $ inputKeyPressed inputb message_cb

		logScroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy logScroll PolicyNever PolicyAutomatic
		containerAdd logScroll logv

		panels <- vPanedNew
		panedPack1 panels logScroll True False
		panedPack2 panels inputv True False

		set w [ containerChild := panels ]
		widgetShowAll w
		Rectangle _ _ _ height <- widgetGetAllocation panels
		panedSetPosition panels (round (0.9 * fromIntegral height))

		-- wait for the widgets to pop up to catch the upcoming messages
		whileM_ (fmap (> 0) eventsPending) $ mainIterationDo False
	
		pure (\entry -> postGUISync $ do
			bufferAdd logb $ renderMessage entry
			-- wait for the buffer to draw // https://stackoverflow.com/a/40917718/4095104
			whileM_ (fmap (> 0) eventsPending) $ mainIterationDo False
			-- scroll the log to the newly received message
			ei <- textBufferGetEndIter logb
			textViewScrollToIter logv ei 0 $ Just (1, 1)
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
	ei <- textBufferGetEndIter tb
	textBufferInsert tb ei s

-- Keypress event handler
inputKeyPressed inputb message_cb = do
	key <- eventKeyName
	mods <- eventModifier
	liftIO $ if (key == "Return") && (notElem Shift mods)
	then do
		t <- bufferGet inputb
		message_cb t
		textBufferSetText inputb ("" :: Text)
		return True
	else return False
