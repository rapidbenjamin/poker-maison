module Socket.Utils where

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

encodeMsg :: Msg -> Text
encodeMsg a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

parseMsg :: Text -> Maybe Msg
parseMsg jsonTxt = decode $ C.pack $ T.unpack jsonTxt

getTimestamp :: UTCTime
getTimestamp = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
