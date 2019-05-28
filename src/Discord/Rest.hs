
module Discord.Rest
    ( CreateMessageRequest(..)
    , DiscordReq
    , DiscordReqErr(..)
    , Request(..)
    , createMessage
    , runDiscordReq
    )
    where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Status as HS
import           Network.HTTP.Req
import           UnliftIO.Exception

import Discord.Gateway (Token(..)) -- TODO: move token to Types or something
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
createMessage channel create = Request (Just channel) $ \token ->
    req POST (baseUrl /: "channels" /~ unSnowflake channel /: "messages") (ReqBodyJson create)
             jsonResponse
             (header "Authorization" ("Bot " <> encodeUtf8 (unToken token)))

data DiscordReqErr = RateLimited (HC.Response ()) | ServerError (HC.Response ()) deriving Show

newtype DiscordReq a = DiscordReq { unDiscordReq :: ExceptT DiscordReqErr IO a } deriving (Functor, Applicative, Monad, MonadError DiscordReqErr, MonadIO)

runDiscordReq :: DiscordReq a -> IO (Either DiscordReqErr a)
runDiscordReq = runExceptT . unDiscordReq

instance MonadHttp DiscordReq where
    handleHttpException e = case e of
        VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException resp _)) ->
            case HC.responseStatus resp of
                HS.Status 429 _ -> liftEither (Left (RateLimited resp))
                HS.Status n   _
                  | n >= 500 && n < 600 -> liftEither (Left (ServerError resp))
                _ -> liftIO $ throwIO e
        _ -> liftIO $ throwIO e


data Request a = Request
    { requestMajor :: Maybe Snowflake
    , requestToReq :: Token -> DiscordReq (JsonResponse a)
    }
