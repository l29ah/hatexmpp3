-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- XEP-0008: IQ-Based Avatars

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Extras.IQAvatar
	( askIQAvatar
	) where

import Data.XML.Types as DXT
import Network.Xmpp.Internal hiding (priority, status)

askIQAvatar jid = sendIQ' Nothing (Just jid) Get Nothing (DXT.Element "query" [("xmlns", [DXT.ContentText "jabber:iq:avatar"])] []) []
