{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk hiding (eventKeyName, eventModifier)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Abstract.Paned
--import Data.DateTime
import Data.Char
import Data.Text

-- Make lines from hatexmpp look beautiful
--pretty :: String -> String
--pretty t = let (x:xs) = words t in
--	(if all isDigit x 
--	then (formatDateTime "[%T] " $ fromSeconds $ read x) ++ unwords xs
--	else t) ++ "\n"
pretty = id

bufferGet :: TextBuffer -> IO String
bufferGet tb = do
	si <- textBufferGetStartIter tb
	ei <- textBufferGetEndIter tb
	textBufferGetText tb si ei True


bufferAdd :: TextBuffer -> String -> IO ()
bufferAdd tb s = do
	t <- bufferGet tb
	textBufferSetText tb (t ++ s)  

-- Keypress event handler
inputKeyPressed inputb e = if (eventKeyName e == "Return") && (notElem Shift $ eventModifier e)
	then do	
		t <- bufferGet inputb
		putStr t
		hFlush stdout
		textBufferSetText inputb ("" :: Text)
		return True
	else return False

main = do
	--args <- initGUI
	unsafeInitGUIForThreadedRTS
	--let (chatfile:_) = args
	
	hSetBuffering stdout $ BlockBuffering Nothing
	hSetBuffering stdin $ NoBuffering --LineBuffering

	w <- windowNew
	onDestroy w mainQuit
	windowSetTitle w ("glovexmpp" :: Text)
	windowSetDefaultSize w 500 500

	logb <- textBufferNew Nothing
	logv <- textViewNewWithBuffer logb
	textViewSetEditable logv False
	textViewSetCursorVisible logv False

	inputb <- textBufferNew Nothing
	inputv <- textViewNewWithBuffer inputb
	textViewSetAcceptsTab inputv True
	onKeyPress inputv $ inputKeyPressed inputb
	panels <- vPanedNew
	panedPack1 panels logv True False
	panedPack2 panels inputv True False

	set w [ containerChild := panels ]
	widgetShowAll w
	Rectangle _ _ _ height <- widgetGetAllocation panels
	print height
	panedSetPosition panels (round (0.9 * fromIntegral height))
	
	-- Inbound messages drawing thread
	forkOS $ forever $ do 
		l <- getLine
		postGUISync $ bufferAdd logb $ pretty l
		ei <- postGUISync $ textBufferGetEndIter logb
		postGUISync $ textViewScrollToIter logv ei 0 Nothing

	mainGUI
