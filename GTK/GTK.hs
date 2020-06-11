module GTK.GTK where

import Control.Concurrent
import Graphics.UI.Gtk

initGTK :: IO ()
initGTK = do
	forkIO $ do
		unsafeInitGUIForThreadedRTS
		mainGUI
	return ()
