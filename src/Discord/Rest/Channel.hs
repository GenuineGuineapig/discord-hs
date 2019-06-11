
module Discord.Rest.Channel
    ( CreateMessageRequest(..)
    , Request(..)
    , createMessage
    )
    where

import           Data.Aeson
import           Data.Text (Text)
import           Network.HTTP.Req

import Discord.Types.Common
import Discord.Types.Rest

data CreateMessageRequest = CreateMessageRequest -- TODO
    { createMessageContent :: Text
    , createMessageNonce   :: Maybe (Snowflake ())
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
createMessage :: Snowflake Channel -> CreateMessageRequest -> Request Channel Message
createMessage channel create =
    mkRequest (baseUrl /: "channels" /~ unSnowflake channel /: "messages")
              (Just channel)
              POST
              (ReqBodyJson create)
              mempty
