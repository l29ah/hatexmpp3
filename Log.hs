module Log where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Map.Strict as MS
import qualified Data.Text as T
import Data.Time.Clock
import Network.Xmpp.Internal hiding (priority, status)

import Config
import Types

putLog :: Jid -> Msg -> UTCTime -> Hate ()
putLog j m t = do
	s <- ask
	liftIO $ print (j, m, t)
	liftIO $ atomically $ do
		ls <- readTVar $ logs s
		case MS.lookup j ls of
			Nothing -> do
				logv <- newTVar $ [(t, m)]
				writeTVar (logs s) $ MS.insert j logv ls
			Just logv -> do
				log <- readTVar logv
				writeTVar logv $ (t, m) : log
	
getLastLogTS :: Jid -> Hate (Maybe UTCTime)
getLastLogTS j = do
	s <- ask
	ls <- readVar $ logs s
	runMaybeT $ do
		logv <- MaybeT $ return $ MS.lookup j ls
		log <- lift $ readVar logv
		pure $ fst $ head log
