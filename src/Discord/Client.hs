
module Discord.Client
    ( Discord(..)
    , Env(..)
    , Handler
    , HasToken(..)
    , HasRateLimits(..)
    , ReconnectPolicy(..)
    , runDiscord
    , runRequest
    , startGateway
    )
    where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative ((<|>))
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import           Data.CaseInsensitive hiding (traverse)
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types.Status as HS
import           Network.HTTP.Req
import qualified Network.WebSockets as WS
import           UnliftIO hiding (Handler)
import           UnliftIO.Concurrent
import           Wuss

import           Discord.Gateway
import           Discord.Rest
import           Discord.Types

type Handler m = Event -> m ()

data ReconnectPolicy =
    ReconnectAlways
  | ReconnectNever
    deriving Show

startGateway :: (HasToken e, HasIncomingEvents e, MonadReader e m, MonadUnliftIO m) => ReconnectPolicy -> Handler m -> m ()
startGateway policy handler = do
    incoming <- view incomingEventsL

    race_ (runGatewayClient policy)
          (forever $ handleEvent incoming)

    where

    handleEvent incoming = do
        event <- readChan incoming
        handler event


type EpochSeconds = Int

getCurrentTimeEpochSeconds :: IO EpochSeconds
getCurrentTimeEpochSeconds = round <$> getPOSIXTime

type Semaphore = MVar ()

newRateLimits :: IO RateLimits
newRateLimits = RateLimits <$> newMVar () <*> newMVar M.empty

data RateLimits = RateLimits
    { globalLimit :: Semaphore
    , routeLimits :: MVar (Map (Url 'Https, Maybe Snowflake) Semaphore)
    }

data RateLimit = RateLimit RateLimitScope RateLimitReset deriving Show

data RateLimitScope = RLGlobal | RLRoute deriving Show

data RateLimitReset = RLResetAfter Int -- millis
                    | RLResetAt EpochSeconds
                    deriving Show

waitRateLimit :: (HasRateLimits e, MonadReader e m, MonadUnliftIO m) => RateLimit -> m ()
waitRateLimit (RateLimit scope reset) =
    case scope of
        RLGlobal -> do
            lock <- view (rateLimitsL . to globalLimit)
            withMVar lock $ \_ -> delayRateLimit reset
        RLRoute -> delayRateLimit reset

delayRateLimit :: MonadIO m => RateLimitReset -> m ()
delayRateLimit (RLResetAfter millis) = liftIO $ threadDelay (millis * 1_000)
delayRateLimit (RLResetAt resetTime) = liftIO $ do
    currentTime <- getCurrentTimeEpochSeconds
    threadDelay ((resetTime - currentTime) * 1_000_000)

parseLimit :: HC.Response a -> Maybe RateLimit
parseLimit response = do
    guard noneRemaining
    reset <- RLResetAfter <$> retryAfter <|> RLResetAt <$> ratelimitReset
    pure (RateLimit scope reset)

    where
    getHeader bs = lookup (mk bs) (HC.responseHeaders response)

    scope = if isGlobal then RLGlobal else RLRoute
    isGlobal = maybe False (== "true") (getHeader "X-RateLimit-Global")
    noneRemaining = maybe True (== "0") (getHeader "X-RateLimit-Remaining")

    retryAfter     = read . BS8.unpack <$> getHeader "Retry-After"       :: Maybe Int
    ratelimitReset = read . BS8.unpack <$> getHeader "X-Ratelimit-Reset" :: Maybe Int


runRequest :: (FromJSON a, HasToken e, HasRateLimits e, MonadReader e m, MonadUnliftIO m) => Request a -> m a
runRequest request = do
    limits <- view rateLimitsL

    (box :: MVar (Either SomeException a)) <- newEmptyMVar

    lock <- getRouteLock limits (requestUrl request) (requestMajor request)

    let go :: (HasRateLimits e, HasToken e, MonadReader e m, MonadUnliftIO m) => m ()
        go = do
            token  <- view tokenL
            readMVar (globalLimit limits)
            result <- runReq defaultHttpConfig (requestAction request token)

            putMVar box (Right (responseBody result))

            traverse_ waitRateLimit (parseLimit (toVanillaResponse result))
                `catch` (\(e :: HttpException) -> case e of
                    VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException resp _)) ->
                        case HS.statusCode (HC.responseStatus resp) of
                            429 -> traverse_ waitRateLimit (parseLimit resp) *> go
                            _   -> tryPutMVar box (Left (SomeException e)) *> throwIO e
                    _ -> tryPutMVar box (Left (SomeException e)) *> throwIO e)
                `catch` (\(e :: SomeException) -> tryPutMVar box (Left e) *> pure ())

    -- TODO: this shouldn't deadlock(?) but double-check
    _ <- forkIO $ withMVar lock $ \_ -> go

    result <- takeMVar box
    case result of
        Left  e -> throwIO e
        Right a -> pure a



getRouteLock :: MonadUnliftIO m => RateLimits -> Url 'Https -> Maybe Snowflake -> m Semaphore
getRouteLock limits route major = do
    modifyMVar (routeLimits limits) $ \routeLocks ->
        case M.lookup (route, major) routeLocks of
            Just semaphore -> pure (routeLocks, semaphore)
            Nothing -> do
                semaphore <- newMVar ()
                pure (M.insert (route, major) semaphore routeLocks, semaphore)

login :: Token -> WS.ClientApp Int {- heartbeat interval -}
login token conn = do
    heartbeatInterval <- receiveHello conn
    writeMessage (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs")) conn
    Ready _ _ _ <- receiveReady conn -- TODO: unpack cache values?
    pure heartbeatInterval

receiveHello :: WS.ClientApp Int
receiveHello conn = do
    msg <- readMessage conn
    case msg of
        Hello interval -> pure interval
        _ -> receiveHello conn -- TODO: exception? unexpected message

receiveReady :: WS.ClientApp Event
receiveReady conn = do
    msg <- readMessage conn
    case msg of
        Dispatch _ event@(Ready _ _ _) -> pure event
        _ -> receiveReady conn -- TODO: exception? unexpected message


runGatewayClient :: (HasToken e, HasIncomingEvents e, MonadReader e m, MonadIO m) => ReconnectPolicy -> m ()
runGatewayClient policy = do
    token    <- view tokenL
    incoming <- view incomingEventsL

    liftIO $ forever $ do
        runSecureClient "gateway.discord.gg" 443 "/" (discordClient token incoming)
            `catch` \(e :: SomeException) -> do
                putStrLn ("Exception in gateway client: " <> show e)
                case policy of
                    ReconnectAlways -> pure ()
                    ReconnectNever  -> throwIO e

        putStrLn "Lost connection. Reconnecting in 5 seconds..."
        threadDelay 5_000_000

-- TODO: restore sequence numbers when reconnecting
discordClient :: Token -> Chan Event -> WS.ClientApp ()
discordClient token incoming conn = do
    heartbeatInterval <- login token conn
    sequenceRef       <- newIORef Nothing

    race_ (heartbeat sequenceRef heartbeatInterval)
          (readIncoming sequenceRef)

    where

    heartbeat :: IORef (Maybe Int) -> Int -> IO ()
    heartbeat sequenceRef heartbeatInterval = forever $ do
        currentSeq <- readIORef sequenceRef
        writeMessage (OutgoingHeartbeat currentSeq) conn
        threadDelay (heartbeatInterval * 1000)

    readIncoming :: IORef (Maybe Int) -> IO ()
    readIncoming sequenceRef = do
        msg <- readMessage conn
        case msg of
            HeartbeatAck -> pure ()
            Dispatch n event -> do
                writeIORef sequenceRef (Just n)
                writeChan incoming event
            t -> putStrLn ("UNHANDLED: " <> show t)
        readIncoming sequenceRef

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> throwString ("Error decoding message: " <> err) -- TODO: create exception type

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

data Env = Env { envToken      :: Token
               , envRateLimits :: RateLimits
               , envIncoming   :: Chan Event
               }

instance HasToken Env where
    tokenL = lens envToken (\x y -> x { envToken = y })

instance HasRateLimits Env where
    rateLimitsL = lens envRateLimits (\x y -> x { envRateLimits = y })

instance HasIncomingEvents Env where
    incomingEventsL = lens envIncoming (\x y -> x { envIncoming = y })

newtype Discord a = Discord { unDiscord :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runDiscord :: Token -> Discord a -> IO a
runDiscord token action = do
    rateLimits <- newRateLimits
    incoming   <- newChan

    runReaderT (unDiscord action) (Env token rateLimits incoming)

class HasToken e where
    tokenL :: Lens' e Token

instance HasToken Token where
    tokenL = id

class HasRateLimits e where
    rateLimitsL :: Lens' e RateLimits

instance HasRateLimits RateLimits where
    rateLimitsL = id

class HasIncomingEvents e where
    incomingEventsL :: Lens' e (Chan Event)

instance HasIncomingEvents (Chan Event) where
    incomingEventsL = id
