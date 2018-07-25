module Socket.Utils where

import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Time.Calendar
import Data.Time.Clock
import Prelude
import Socket.Types
import Text.Pretty.Simple (pPrint)

encodeMsgToJSON :: MsgOut -> Text
encodeMsgToJSON a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

encodeMsgX :: MsgIn -> Text
encodeMsgX a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

parseMsgFromJSON :: Text -> Maybe MsgIn
parseMsgFromJSON jsonTxt = decode $ C.pack $ T.unpack jsonTxt

getTimestamp :: UTCTime
getTimestamp = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
