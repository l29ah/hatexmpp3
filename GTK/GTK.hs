module GTK.GTK where

import Control.Concurrent
import Control.Monad
import Graphics.UI.Gtk

initGTK :: IO ()
initGTK = do
	forkIO $ do
		unsafeInitGUIForThreadedRTS
		mainGUI
	return ()
