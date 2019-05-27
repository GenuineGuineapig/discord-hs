
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
import           Control.Monad.Reader
import qualified Network.WebSockets as WS
import           UnliftIO hiding (Handler)
import           UnliftIO.Concurrent
import           Wuss

import           Discord.Gateway


type Handler = Event -> Discord ()


data ReconnectPolicy =
    ReconnectAlways
  | ReconnectNever
    deriving Show

startDiscord :: MonadIO m => ReconnectPolicy -> Token -> Handler -> m ()
startDiscord policy token handler = liftIO $ do
    -- TODO rest requests
    gtid <- runGatewayClient policy token handler

    threadDelay maxBound `finally` killThread gtid -- TODO

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


-- TODO: rate limits
runGatewayClient :: ReconnectPolicy -> Token -> Handler -> IO ThreadId
runGatewayClient policy token handler = forkWithUnmask $ \restore ->
    forever $ do
        restore (runSecureClient "gateway.discord.gg" 443 "/" (discordClient token handler))
            `catch` \(e :: SomeException) -> do
                putStrLn ("Exception in gateway client: " <> show e)
                case policy of
                    ReconnectAlways -> pure ()
                    ReconnectNever  -> throwIO e

        putStrLn "Lost connection. Reconnecting in 5 seconds..."
        threadDelay 5_000_000

    where

discordClient :: Token -> Handler -> WS.ClientApp ()
discordClient token handler conn = do
    heartbeatInterval <- login token conn
    sequenceRef       <- newIORef Nothing

    race_ (heartbeat sequenceRef heartbeatInterval)
          (eventLoop sequenceRef)

    where

    heartbeat :: IORef (Maybe Int) -> Int -> IO ()
    heartbeat sequenceRef heartbeatInterval = forever $ do
        currentSeq <- readIORef sequenceRef
        writeMessage (OutgoingHeartbeat currentSeq) conn
        threadDelay (heartbeatInterval * 1000)

    eventLoop :: IORef (Maybe Int) -> IO ()
    eventLoop sequenceRef = do
        msg <- readMessage conn
        case msg of
            HeartbeatAck -> pure ()
            Dispatch n event -> do
                writeIORef sequenceRef (Just n)
                runDiscord (handler event) (Env conn)
            t -> putStrLn ("UNHANDLED: " <> show t)
        eventLoop sequenceRef

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> throwString ("Error decoding message: " <> err) -- TODO: create exception type

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

data Env = Env { envConn :: WS.Connection }

newtype Discord a = Discord { unDiscord :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runDiscord :: Discord a -> Env -> IO a
runDiscord action env = runReaderT (unDiscord action) env

instance MonadDiscord Discord where
    receive = do
        conn <- asks envConn
        liftIO $ readMessage conn
    send msg = do
        conn <- asks envConn
        liftIO $ writeMessage msg conn

class MonadDiscord m where
    receive :: m GatewayMessage
    send    :: GatewayRequest -> m ()
    --sendRest    :: Request a -> m a
