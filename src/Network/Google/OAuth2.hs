{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeApplications, TypeOperators #-}

module Network.Google.OAuth2
    ( getTokenInfo
    , tokenInfo
    , baseUrl
    , Api
    , TokenInfo
    , tiIss
    , tiAzp
    , tiAud
    , tiSub
    , tiEmail
    , tiEmailVerified
    , tiIat
    , tiExp
    , tiJti
    , tiAlg
    , tiKid
    , tiTyp
    ) where

import Data.Aeson
    ( FromJSON, camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, parseJSON )
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Get, JSON, QueryParam, (:>))
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, mkClientEnv, runClientM)

getTokenInfo :: String -> IO (Maybe TokenInfo)
getTokenInfo tkn = do
    mgr <- newTlsManager
    let env = mkClientEnv mgr baseUrl
    r <- runClientM (tokenInfo $ Just tkn) env
    pure $ either (const Nothing) Just r

tokenInfo :: Maybe String -> ClientM TokenInfo
tokenInfo = client $ Proxy @Api

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "www.googleapis.com" 443 ""

type Api = "oauth2" :> "v3" :> "tokeninfo" :> QueryParam "id_token" String :> Get '[JSON] TokenInfo

data TokenInfo = TokenInfo
    { tiIss :: String
    , tiAzp :: String
    , tiAud :: String
    , tiSub :: String
    , tiEmail :: String
    , tiEmailVerified :: String
    , tiIat :: String
    , tiExp :: String
    , tiJti :: String
    , tiAlg :: String
    , tiKid :: String
    , tiTyp :: String
    } deriving (Generic)

instance FromJSON TokenInfo where
    parseJSON = genericParseJSON $ defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 2
        }
