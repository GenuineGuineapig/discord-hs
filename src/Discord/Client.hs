
{-# language GADTs #-}

module Discord.Client
    ( Discord(..)
    , Env(..)
    , Handler
    , MonadDiscord(..)
    , ReconnectPolicy(..)
    , runDiscord
    , startDiscord
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Applicative ((<|>))
import           Control.Monad.Reader
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.CaseInsensitive
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Req
import qualified Network.WebSockets as WS
import           UnliftIO hiding (Handler)
import           UnliftIO.Concurrent
import           Wuss

import           Discord.Gateway
import           Discord.Rest
import           Discord.Types


type Handler = Event -> Discord ()

data ReconnectPolicy =
    ReconnectAlways
  | ReconnectNever
    deriving Show

startDiscord :: MonadIO m => ReconnectPolicy -> Token -> Handler -> m ()
startDiscord policy token handler = liftIO $ do
    requestChan <- newChan

    -- TODO rest requests
    race_ (runGatewayClient policy token handler requestChan)
          (runRestClient requestChan token)


newLimits :: IO RateLimits
newLimits = RateLimits <$> newIORef 0 <*> newIORef M.empty

type EpochSeconds = Int

-- EpochSeconds values represent when the rate limit will expire
data RateLimits = RateLimits
    { globalRateLimit :: IORef EpochSeconds
    , majorRateLimits  :: IORef (Map Snowflake EpochSeconds)
    }

getCurrentTimeEpochSeconds :: IO Int
getCurrentTimeEpochSeconds = round <$> getPOSIXTime

-- threadDelay until the applicable rate limits expire
waitForRateLimits :: RateLimits -> Maybe Snowflake -> IO ()
waitForRateLimits limits major = do
    globalLimit <- readIORef (globalRateLimit limits)
    majorLimits <- readIORef (majorRateLimits limits)

    let majorLimit  = flip M.lookup majorLimits =<< major
        latestLimit = foldr max globalLimit majorLimit

    currentTime <- getCurrentTimeEpochSeconds


    when (latestLimit > currentTime)
        (threadDelay ((latestLimit - currentTime) * 1_000_000))

    -- clean old snowflake limits
    modifyIORef (majorRateLimits limits) (M.filter (> currentTime))

--   http.client
-- responseHeaders :: Response _ -> [Header]
-- type Header = (CI ByteString, ByteString)
--   req
-- responseHeader :: response -> ByteString -> Maybe ByteString

recordLimits :: (ByteString -> Maybe ByteString) -- function to extract headers
             -> (Maybe Snowflake) -> RateLimits -> IO ()
recordLimits getHeader major limits = do
    currentTimeSeconds <- getCurrentTimeEpochSeconds

    let isGlobal      = maybe False (== "true") (getHeader "X-RateLimit-Global")
        noneRemaining = maybe True  (== "0")    (getHeader "X-RateLimit-Remaining")

        retryAfter = read . BS8.unpack <$> getHeader "Retry-After"       :: Maybe Int
        reset      = read . BS8.unpack <$> getHeader "X-Ratelimit-Reset" :: Maybe Int

        limitTime  = fromMaybe (error "missing at least one of retry-after or ratelimit-reset") (fmap (\millis -> currentTimeSeconds + (millis `div` 1000)) retryAfter <|> reset)

    when noneRemaining $
        case (isGlobal, major) of
            (True , _     ) -> writeIORef (globalRateLimit limits) limitTime
            (False, Just m) -> modifyIORef (majorRateLimits limits) (M.insert m limitTime)
            (_    , _     ) -> error "missing major for rate limit"

headerLookupHC :: HC.Response a -> (ByteString -> Maybe ByteString)
headerLookupHC resp bs = lookup (mk bs) (HC.responseHeaders resp)

runRestClient :: Chan SomeRequest -> Token -> IO ()
runRestClient requestChan token = do
    limits <- newLimits

    forever (execRequest limits =<< readChan requestChan)

    where

    execRequest :: RateLimits -> SomeRequest -> IO ()
    execRequest limits raw@(SomeRequest request respVar) = do
        let major = requestMajor request

        waitForRateLimits limits major

        result <- runDiscordReq (requestToReq request token)
        case result of
            Left err  -> case err of
                RateLimited resp -> recordLimits (headerLookupHC resp) major limits >> execRequest limits raw
                ServerError _    -> threadDelay 5_000_000 >> execRequest limits raw -- TODO: print server error?
            Right resp -> recordLimits (responseHeader resp) major limits >> putMVar respVar (responseBody resp)

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


runGatewayClient :: ReconnectPolicy -> Token -> Handler -> Chan SomeRequest -> IO ThreadId
runGatewayClient policy token handler requestChan =
    forever $ do
        runSecureClient "gateway.discord.gg" 443 "/" (discordClient token handler requestChan)
            `catch` \(e :: SomeException) -> do
                putStrLn ("Exception in gateway client: " <> show e)
                case policy of
                    ReconnectAlways -> pure ()
                    ReconnectNever  -> throwIO e

        putStrLn "Lost connection. Reconnecting in 5 seconds..."
        threadDelay 5_000_000

discordClient :: Token -> Handler -> Chan SomeRequest -> WS.ClientApp ()
discordClient token handler requestChan conn = do
    heartbeatInterval <- login token conn
    sequenceRef       <- newIORef Nothing

    let env = Env requestChan

    race_ (heartbeat sequenceRef heartbeatInterval)
          (eventLoop sequenceRef env)

    where

    heartbeat :: IORef (Maybe Int) -> Int -> IO ()
    heartbeat sequenceRef heartbeatInterval = forever $ do
        currentSeq <- readIORef sequenceRef
        writeMessage (OutgoingHeartbeat currentSeq) conn
        threadDelay (heartbeatInterval * 1000)

    eventLoop :: IORef (Maybe Int) -> Env -> IO ()
    eventLoop sequenceRef env = do
        msg <- readMessage conn
        case msg of
            HeartbeatAck -> pure ()
            Dispatch n event -> do
                writeIORef sequenceRef (Just n)
                runDiscord (handler event) env
            t -> putStrLn ("UNHANDLED: " <> show t)
        eventLoop sequenceRef env

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> throwString ("Error decoding message: " <> err) -- TODO: create exception type

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

data SomeRequest where
    SomeRequest :: FromJSON a => Request a -> MVar a -> SomeRequest

data Env = Env { envRequests :: Chan SomeRequest }

newtype Discord a = Discord { unDiscord :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runDiscord :: Discord a -> Env -> IO a
runDiscord action env = runReaderT (unDiscord action) env

instance MonadDiscord Discord where
    makeRequest request = do
        result      <- newEmptyMVar
        requestChan <- asks envRequests
        writeChan requestChan (SomeRequest request result)
        readMVar result

class MonadDiscord m where
    makeRequest :: FromJSON a => Request a -> m a
