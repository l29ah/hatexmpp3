-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- XEP-0153: vCard-Based Avatars

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Extras.VCardAvatar
	( askvCard
	) where

import Data.Default
import Data.Maybe
import Data.Text as T
import Data.XML.Types as DXT
import Network.Xmpp.Extras.DateTime
import Network.Xmpp.Internal hiding (priority, status)

--askvCard :: Jid -> Session -> IO (Either XmppFailure IQResponse)
askvCard jid = sendIQ' Nothing (Just jid) Get Nothing (DXT.Element "vCard" [("xmlns", [DXT.ContentText "vcard-temp"])] []) []
