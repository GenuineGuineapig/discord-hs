
{-# language ImpredicativeTypes #-}
{-# language TemplateHaskell    #-}

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
import           Control.Monad.Reader hiding (Reader, ask, runReader)
import qualified Data.ByteString.Char8 as BS8
import           Data.CaseInsensitive
import           Data.Coerce
import           Data.Foldable
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Status as HS
import           Network.HTTP.Req
import           Polysemy
import           Polysemy.IdempotentLowering
import           Polysemy.Reader

import Discord.Rest.Channel
import Discord.Types.Common

  -- TODO: remove
import qualified Data.Text.IO as TIO


baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"

data Request m a where
    CreateMessage :: Snowflake Channel -> CreateMessageRequest -> Request m Message

data RateLimiting m a where
    WithRateLimits :: HttpResponse resp => Text -> Maybe (Snowflake x) -> m resp -> RateLimiting m resp

makeSem ''RateLimiting
makeSem ''Request

test2 :: Member RateLimiting r => Sem r (JsonResponse ())
test2 = do
    withRateLimits "a" Nothing $ do
        (x :: JsonResponse ()) <- undefined
        pure x

testy2 :: IO (JsonResponse ())
testy2 = do
    runIt <- nat runM .@! runRateLimiting
    res <- runIt test2
    pure res


test1 :: Member Request r => Sem r Message
test1 = createMessage (Snowflake 582769331897761792) (CreateMessageRequest "something" Nothing Nothing Nothing)

testy1 :: IO Message
testy1 = do
    token <- Token <$> TIO.readFile "../discord.auth"
    runIt <- nat runM .@! runRateLimiting
    let runIt' = runIt . runRequest token
    runIt' test1

runRequest :: (Member RateLimiting r, Member (Lift IO) r) => Token -> Sem (Request ': r) a -> Sem r a
runRequest token = interpret $ \r -> do
    let headers :: Option 'Https
        headers = header "Authorization" ("Bot " <> encodeUtf8 (unToken token))

    case r of
        CreateMessage channel create -> fmap responseBody . withRateLimits "CreateMessage" (Just channel) $ runReq defaultHttpConfig $
            req POST
                (baseUrl /: "channels" /~ unSnowflake channel /: "messages")
                (ReqBodyJson create)
                jsonResponse
                headers


runRateLimiting :: (Member (Lift IO) r) => (forall x. Sem r x -> IO x) -> IO (forall a. Sem (RateLimiting ': r) a -> Sem r a)
runRateLimiting lower = do
    limits <- newRateLimits

    fixedNat $ \me -> interpretH $ \case
        WithRateLimits url major action -> do
            act <- runT action
            ins <- getInspectorT

            lock <- sendM @IO $ getRouteLock limits url major

            (box :: MVar (Either SomeException a)) <- sendM @IO newEmptyMVar

            let go :: IO ()
                go = do
                    -- Ensure we haven't yet hit the global rate limit
                    readMVar (globalLock limits)

                    blah <- lower (me act)
                    let Just result = inspect ins blah
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
                Right a -> pureT a

getRouteLock :: RateLimits -> Text -> Maybe (Snowflake ty) -> IO Lock
getRouteLock limits route major = do
    modifyMVar (routeLocks limits) $ \locks ->
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
