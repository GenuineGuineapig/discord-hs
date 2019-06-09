
module Discord.Rest
    ( CreateMessageRequest(..)
    , Request(..)
    , createMessage
    )
    where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Req

import Discord.Types

baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"


data CreateMessageRequest = CreateMessageRequest -- TODO
    { createMessageContent :: Text
    , createMessageNonce   :: Maybe Snowflake
    , createMessageTts     :: Maybe Bool
    , createMessageEmbed   :: Maybe Embed
    -- TODO createMessageFile        :: ?
    -- TODO createMessagePayloadJson :: ?
    } deriving Show

instance ToJSON CreateMessageRequest where
    toJSON msg = object [ "content" .= createMessageContent msg
                        , "nonce"   .= createMessageNonce msg
                        , "tts"     .= createMessageTts msg
                        , "embed"   .= createMessageEmbed msg
                        ]

-- POST /channels/{channel.id}/messages
createMessage :: Snowflake -> CreateMessageRequest -> Request Message
createMessage channel create =
    mkRequest (baseUrl /: "channels" /~ unSnowflake channel /: "messages")
              (Just channel)
              POST
              (ReqBodyJson create)
              mempty


mkRequest :: (FromJSON a, HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) => Url 'Https -> Maybe Snowflake -> method -> body -> Option 'Https -> Request a
mkRequest url major method body options =
    Request { requestUrl    = url
            , requestMajor  = major
            , requestAction = \token -> req method url body jsonResponse (options <> (header "Authorization" ("Bot " <> encodeUtf8 (unToken token))))
            }

data Request a = Request { requestUrl    :: Url 'Https
                         , requestMajor  :: Maybe Snowflake
                         , requestAction :: Token -> Req (JsonResponse a)
                         }
