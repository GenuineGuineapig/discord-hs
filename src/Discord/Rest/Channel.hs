
module Discord.Rest.Channel
    ( CreateMessageRequest(..)
    )
    where

import           Data.Aeson
import           Data.Text (Text)

import Discord.Types.Common

data CreateMessageRequest = CreateMessageRequest -- TODO
    { createMessageContent :: Text
    , createMessageNonce   :: Maybe (Snowflake ())
    , createMessageTts     :: Maybe Bool
    , createMessageEmbed   :: Maybe MsgEmbed
    -- TODO createMessageFile        :: ?
    -- TODO createMessagePayloadJson :: ?
    } deriving Show

instance ToJSON CreateMessageRequest where
    toJSON msg = object [ "content" .= createMessageContent msg
                        , "nonce"   .= createMessageNonce msg
                        , "tts"     .= createMessageTts msg
                        , "embed"   .= createMessageEmbed msg
                        ]
