-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- XEP-0082: XMPP Date and Time Profiles

module Network.Xmpp.Extras.DateTime where

import Data.Time

-- |No milliseconds yet as `formatTime` lacks the required feature
toDateTime :: UTCTime -> String
toDateTime t = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
