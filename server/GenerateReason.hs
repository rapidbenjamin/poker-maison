{-# LANGUAGE OverloadedStrings #-}

module GenerateReason where

import Control.Concurrent.Async
import Database.Redis (defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Prelude
import qualified System.Remote.Monitoring as EKG
import Data.Text.Encoding as TSE

import API
import Database
import Env
import Socket
import Web.JWT (secret)
import Servant.Auth.Server

import           Reason          (Spec (Spec), specsToDir, toReasonDecoderSource,
                               toReasonTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Reason  (ReasonType, Proxy (Proxy), defReasonImports,
                               generateReasonForAPI)
import Servant

import Data.Proxy
import Reason
import Data.Text hiding (intercalate, map)
import Types
import Data.List (intercalate, map)

main :: IO ()
main =
  writeFile "../client/app/Api.re" $ intercalate "\n\n" $ map unpack code
  where 
    code = 
      defReasonImports
       : toReasonTypeSource (Proxy :: Proxy Username)
       : toReasonDecoderSource (Proxy :: Proxy Username)
       : toReasonEncoderSource (Proxy :: Proxy Username)

       : toReasonTypeSource (Proxy :: Proxy Login)
       : toReasonEncoderSource (Proxy :: Proxy Login)

       : toReasonTypeSource    (Proxy :: Proxy ReturnToken)
       : toReasonDecoderSource (Proxy :: Proxy ReturnToken)

       : toReasonTypeSource    (Proxy :: Proxy UserProfile)
       : toReasonDecoderSource (Proxy :: Proxy UserProfile)

       : (generateReasonForAPI api)