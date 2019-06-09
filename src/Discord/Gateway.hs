
module Discord.Gateway
    ( HasIncomingEvents(..)
    , startGateway
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Lens
import           Control.Monad.Reader
import qualified Network.WebSockets as WS
import           UnliftIO hiding (Handler)
import           UnliftIO.Concurrent
import           Wuss

import Discord.Types.Common
import Discord.Types.Gateway


class HasIncomingEvents e where
    incomingEventsL :: Lens' e (Chan Event)

instance HasIncomingEvents (Chan Event) where
    incomingEventsL = id

startGateway :: (HasToken e, HasIncomingEvents e, MonadReader e m, MonadUnliftIO m) => ReconnectPolicy -> (Event -> m ()) -> m ()
startGateway policy handler = do
    incoming <- view incomingEventsL

    race_ (runGatewayClient policy)
          (forever $ handleEvent incoming)

    where

    handleEvent incoming = do
        event <- readChan incoming
        handler event


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
