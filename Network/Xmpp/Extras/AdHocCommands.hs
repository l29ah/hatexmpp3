-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- XEP-0050: Ad-Hoc Commands

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Extras.AdHocCommands where

import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.XML.Types as DXT
import Network.Xmpp.Extras.DateTime
import Network.Xmpp.Internal hiding (priority, status)

rightToMaybe = fixDnsResult

askCommandList :: Jid -> Session -> IO (Either IQSendError IQResponse)
askCommandList jid = sendIQ' Nothing (Just jid) Get Nothing (DXT.Element "query" [
	("xmlns",	[DXT.ContentText "http://jabber.org/protocol/disco#items"]),
	("node",	[DXT.ContentText "http://jabber.org/protocol/commands"])] []) []

commandList jid s = do
	list <- askCommandList jid s
	return $ fromJust $ do
		iqresp <- rightToMaybe list
		iqresult <- case iqresp of
			IQResponseResult a -> Just a
			_ -> Nothing
		payload <- iqResultPayload iqresult
		sequence $ map (\(NodeElement a) -> do
			node <- attributeText "node" a
			name <- attributeText "name" a
			return (node, name)) $ elementNodes payload

askCommandInfo :: Jid -> Text -> Session -> IO (Either IQSendError IQResponse)
askCommandInfo jid command = sendIQ' Nothing (Just jid) Get Nothing (DXT.Element "query" [
	("xmlns",	[DXT.ContentText "http://jabber.org/protocol/disco#info"]),
	("node",	[DXT.ContentText command])] []) []

execute :: Jid -> Text -> Session -> IO (Either IQSendError IQResponse)
execute jid command = sendIQ' Nothing (Just jid) Set Nothing (DXT.Element "command" [
	("xmlns",	[DXT.ContentText "http://jabber.org/protocol/commands"]),
	("node",	[DXT.ContentText command]),
	("action",	[DXT.ContentText "execute"])] []) []
