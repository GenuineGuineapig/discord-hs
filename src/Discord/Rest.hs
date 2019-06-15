
{-# language TemplateHaskell #-}

module Discord.Rest
    ( RateLimits(..)
    , Request(..)
    , createMessage
    , newRateLimits
    , runRequest
    , runRateLimiting
    , withRateLimits
    )
    where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception hiding (bracket)
import           Control.Monad.Reader hiding (Reader, ask)
import qualified Data.ByteString.Char8 as BS8
import           Data.CaseInsensitive
import           Data.Coerce
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Status as HS
import           Network.HTTP.Req
import           Polysemy
import           Polysemy.Reader

import Discord.Rest.Channel
import Discord.Types.Common


baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"

data Request m a where
    CreateMessage :: Snowflake Channel -> CreateMessageRequest -> Request m Message

data RateLimiting m a where
    WithRateLimits :: HttpResponse resp => Text -> Maybe (Snowflake x) -> IO resp -> RateLimiting m resp

makeSem ''RateLimiting
makeSem ''Request

runRequest :: (Member (Reader Token) r) => Sem (Request ': r) a -> Sem (RateLimiting ': r) a
runRequest = reinterpret $ \r -> do
    token <- ask @Token

    let headers :: Option 'Https
        headers = header "Authorization" ("Bot " <> encodeUtf8 (unToken token))

    case r of
        CreateMessage channel create -> fmap responseBody . withRateLimits "CreateMessage" (Just channel) $ runReq defaultHttpConfig $
            req POST
                (baseUrl /: "channels" /~ unSnowflake channel /: "messages")
                (ReqBodyJson create)
                jsonResponse
                headers


runRateLimiting :: (Member (Lift IO) r) => Sem (RateLimiting ': r) a -> Sem (Reader RateLimits ': r) a
runRateLimiting = reinterpret $ \case
    WithRateLimits url major action -> do
        limits <- ask @RateLimits

        lock <- getRouteLock url major

        (box :: MVar (Either SomeException a)) <- sendM newEmptyMVar

        let go :: IO ()
            go = do
                -- Ensure we haven't yet hit the global rate limit
                readMVar (globalLock limits)

                result <- action
                putMVar box (Right result)

                traverse_ (waitRateLimit limits) (parseLimit (toVanillaResponse result))
                    `catch` (\(e :: HttpException) -> case e of
                        VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException resp _)) ->
                            case HS.statusCode (HC.responseStatus resp) of
                                429 -> traverse_ (waitRateLimit limits) (parseLimit resp) *> go
                                _   -> tryPutMVar box (Left (SomeException e)) *> throwIO e
                        _ -> tryPutMVar box (Left (SomeException e)) *> throwIO e)
                    `catch` (\(e :: SomeException) -> void $ tryPutMVar box (Left e))

        -- TODO: this shouldn't deadlock(?) but double-check
        _ <- sendM $ forkIO $ withMVar lock (const go)

        result <- sendM $ takeMVar box
        case result of
            Left  e -> sendM $ throwIO e
            Right a -> pure a

getRouteLock :: (Member (Reader RateLimits) r, Member (Lift IO) r) => Text -> Maybe (Snowflake ty) -> Sem r Lock
getRouteLock route major = do
    limits <- ask @RateLimits

    sendM @IO $ modifyMVar (routeLocks limits) $ \locks ->
        case M.lookup (route, coerce major) locks of
            Just lock -> pure (locks, lock)
            Nothing -> do
                lock <- newMVar ()
                pure (M.insert (route, coerce major) lock locks, lock)

parseLimit :: HC.Response a -> Maybe RateLimit
parseLimit response = do
    guard noneRemaining
    reset <- RLResetAfter <$> retryAfter <|> RLResetAt <$> ratelimitReset
    pure (RateLimit scope reset)

    where
    getHeader bs = lookup (mk bs) (HC.responseHeaders response)

    scope = case getHeader "X-Ratelimit-Global" of
        Just "true" -> RLGlobal
        _           -> RLRoute
    noneRemaining = maybe True (== "0") (getHeader "X-RateLimit-Remaining")

    retryAfter     = read . BS8.unpack <$> getHeader "Retry-After"       :: Maybe Int
    ratelimitReset = read . BS8.unpack <$> getHeader "X-Ratelimit-Reset" :: Maybe Int

waitRateLimit :: RateLimits -> RateLimit -> IO ()
waitRateLimit limits (RateLimit scope reset) =
    case scope of
        RLGlobal -> withMVar (globalLock limits) (const (delayRateLimit reset))
        RLRoute -> delayRateLimit reset

delayRateLimit :: RateLimitReset -> IO ()
delayRateLimit (RLResetAfter millis) = liftIO $ threadDelay (millis * 1_000)
delayRateLimit (RLResetAt resetTime) = liftIO $ do
    currentTime <- getCurrentTimeEpochSeconds
    threadDelay ((resetTime - currentTime) * 1_000_000)

newRateLimits :: IO RateLimits
newRateLimits = RateLimits <$> newMVar () <*> newMVar M.empty

type Lock = MVar ()

data RateLimits = RateLimits
    { globalLock :: Lock
    , routeLocks :: MVar (Map (Text, Maybe SomeSnowflake) Lock)
    }

data RateLimit = RateLimit RateLimitScope RateLimitReset deriving Show

data RateLimitScope = RLGlobal | RLRoute deriving Show

data RateLimitReset = RLResetAfter Int -- millis
                    | RLResetAt EpochSeconds
                    deriving Show


type EpochSeconds = Int

getCurrentTimeEpochSeconds :: IO EpochSeconds
getCurrentTimeEpochSeconds = round <$> getPOSIXTime
