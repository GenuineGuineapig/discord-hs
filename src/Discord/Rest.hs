
{-# language TemplateHaskell #-}

module Discord.Rest
    ( RateLimits(..)
    , Request(..)
    , createMessage
    , requestToIO
    , usingRateLimits
    )
    where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception hiding (bracket)
import           Control.Monad
import qualified Data.ByteString.Char8 as BS8
import           Data.CaseInsensitive
import           Data.Coerce
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Status as HS
import           Network.HTTP.Req
import           Polysemy
import           Polysemy.State

import Discord.Rest.Channel
import Discord.Types.Common


baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"

data Request m a where
    CreateMessage :: Snowflake Channel -> CreateMessageRequest -> Request m Message

makeSem ''Request


requestToIO :: Member (Embed IO) r => Token -> Sem (Request ': r) a -> Sem r a
requestToIO token = evalState newRateLimits . reinterpret (\case
    CreateMessage channel create ->
        fmap responseBody . usingRateLimits "CreateMessage" (Just channel) $ runReq defaultHttpConfig $
            req POST
                (baseUrl /: "channels" /~ unSnowflake channel /: "messages")
                (ReqBodyJson create)
                jsonResponse
                headers)
    where
    headers :: Option 'Https
    headers = header "Authorization" ("Bot " <> encodeUtf8 (unToken token))


usingRateLimits :: ( Member (Embed IO) r
                   , Member (State RateLimits) r
                   , HttpResponse resp
                   ) => Text -> Maybe (Snowflake x) -> IO resp -> Sem r resp
usingRateLimits url major action = do
    RateLimits global routes <- get
    currentTime <- embed $ getCurrentTimeEpochSeconds

    let resetTime = max global (fromMaybe 0 (M.lookup (url, coerce major) routes))
    embed $ threadDelay ((resetTime - currentTime) * 1_000_000)

    -- result :: Either RateLimit resp
    result <- embed $ fmap pure action
                  `catch` (\(e :: HttpException) -> case e of
                      VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException resp _)) ->
                          case HS.statusCode (HC.responseStatus resp) of
                              429 -> case parseLimit currentTime resp of
                                  Nothing    -> throwIO e
                                  Just limit -> pure (Left limit)
                              _   -> throwIO e
                      _ -> throwIO e)

    case result of
        Left limit -> updateLimits limit *> usingRateLimits url major action

        Right resp -> do
            afterTime <- embed $ getCurrentTimeEpochSeconds

            let limit = parseLimit afterTime (toVanillaResponse resp)

            traverse_ updateLimits limit

            pure resp
    where
    updateLimits (RateLimit RLGlobal time) =
        modify (\limits -> limits { globalReset = time})
    updateLimits (RateLimit RLRoute time) =
        modify (\limits -> limits { routeResets = (M.insert (url, coerce major) time (routeResets limits))})

parseLimit :: EpochSeconds -> HC.Response a -> Maybe RateLimit
parseLimit currentTime response = do
    guard noneRemaining
    reset <- (\millis -> ceiling (fromIntegral millis / 1000 :: Double) + currentTime) <$> retryAfter <|> ratelimitReset
    pure (RateLimit scope reset)

    where
    getHeader bs = lookup (mk bs) (HC.responseHeaders response)

    scope = case getHeader "X-Ratelimit-Global" of
        Just "true" -> RLGlobal
        _           -> RLRoute
    noneRemaining = maybe True (== "0") (getHeader "X-RateLimit-Remaining")

    retryAfter     = read . BS8.unpack <$> getHeader "Retry-After"       :: Maybe Int
    ratelimitReset = read . BS8.unpack <$> getHeader "X-Ratelimit-Reset" :: Maybe Int

newRateLimits :: RateLimits
newRateLimits = RateLimits 0 M.empty

-- Rate limits for discord can be global or route-level.
-- Route-level limits can be specific to a "major" parameter (snowflake): channel, guild, or webhook
data RateLimits = RateLimits
    { globalReset :: EpochSeconds
    , routeResets :: Map (Text, Maybe SomeSnowflake) EpochSeconds
    }

data RateLimit = RateLimit RateLimitScope EpochSeconds deriving Show

data RateLimitScope = RLGlobal | RLRoute deriving Show

data RateLimitReset = RLResetAfter Int -- millis
                    | RLResetAt EpochSeconds
                    deriving Show


type EpochSeconds = Int

getCurrentTimeEpochSeconds :: IO EpochSeconds
getCurrentTimeEpochSeconds = round <$> getPOSIXTime
