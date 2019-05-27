
module Discord.Rest
    ( Request(..)
    , CreateMessageRequest(..)
    , createMessage
    )
    where

import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

import Discord.Gateway (Token(..)) -- TODO: move token to Types or something
import Discord.Types

baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"


-- content	string	the message contents (up to 2000 characters)
-- nonce	snowflake	a nonce that can be used for optimistic message sending
-- tts	boolean	true if this is a TTS message
-- file	file contents	the contents of the file being sent
-- embed	embed object	embedded rich content
-- payload_json	string	JSON encoded body of any additional request fields.

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
createMessage channel create = Request $ \token ->
    runReq' $ req POST
                  (baseUrl /: "channels" /~ unSnowflake channel /: "messages") (ReqBodyJson create)
                  jsonResponse
                  (header "Authorization" ("Bot " <> encodeUtf8 (unToken token)))

newtype Request a = Request { runRequest :: Token -> IO a }

runReq' :: FromJSON a => Req (JsonResponse a) -> IO a
runReq' = runReq defaultHttpConfig . fmap responseBody


    --gatewayUrl <- runReq' $ req GET (baseUrl /: "gateway") NoReqBody jsonResponse mempty
