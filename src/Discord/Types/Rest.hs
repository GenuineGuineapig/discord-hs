
module Discord.Types.Rest
    ( baseUrl
    , Request(..)
    , mkRequest
    )
    where

import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

import Discord.Types.Common

baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"


mkRequest :: (FromJSON a, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) => Url 'Https -> Maybe (Snowflake ty) -> method -> body -> Option 'Https -> Request ty a
mkRequest url major method body options = Request
    { requestUrl    = url
    , requestMajor  = major
    , requestAction = \token -> req method url body jsonResponse (options <> header "Authorization" ("Bot " <> encodeUtf8 (unToken token)))
    }

data Request ty a = Request
    { requestUrl    :: Url 'Https
    , requestMajor  :: Maybe (Snowflake ty)
    , requestAction :: Token -> Req (JsonResponse a)
    }
