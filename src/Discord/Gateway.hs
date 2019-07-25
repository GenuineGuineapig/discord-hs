
{-# language ImpredicativeTypes #-}
{-# language TemplateHaskell    #-}

module Discord.Gateway
    ( Gateway(..)
    , receiveEvent
    , runGateway
    , runGatewayAsInput
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Reader hiding (Reader)
import qualified Network.WebSockets as WS
import           Polysemy
import           Polysemy.Input
import           UnliftIO hiding (Handler, bracket)
import           UnliftIO.Concurrent
import           Wuss

import Discord.Types.Common
import Discord.Types.Gateway


data Gateway m a where
    ReceiveEvent :: Gateway m (Maybe Event)

makeSem ''Gateway

runGatewayAsInput :: Member (Input (Maybe Event)) r => Sem (Gateway ': r) a -> Sem r a
runGatewayAsInput = interpret $ \case
    ReceiveEvent -> input

runGateway :: (Member (Embed IO) r) => Token -> TMChan Event -> Sem (Gateway ': r) a -> Sem r a
runGateway token incoming = undefined {- do

    _ <- forkIO $ runGatewayClient token incoming

    nat $ interpret $ \case
        ReceiveEvent -> do
            embed @IO (atomically (readTMChan incoming)) -}

login :: Token -> WS.ClientApp Int {- heartbeat interval -}
login token conn = do
    heartbeatInterval <- receiveHello conn
    writeMessage (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs") Nothing Nothing Nothing Nothing) conn
    Ready{} <- receiveReady conn -- TODO: unpack cache values?
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
        Dispatch _ event@Ready{} -> pure event
        _ -> receiveReady conn -- TODO: exception? unexpected message


runGatewayClient :: Token -> TMChan Event -> IO ()
runGatewayClient token incoming = do
    flip finally (atomically (closeTMChan incoming)) $
        forever $ do
            runSecureClient "gateway.discord.gg" 443 "/" (discordClient token incoming)
                `catch` \(e :: SomeException) -> do -- TODO: make sure we handle async exceptions here
                    putStrLn ("Exception in gateway client: " <> show e)

            putStrLn "Lost connection. Reconnecting in 5 seconds..."
            threadDelay 5_000_000

-- TODO: restore sequence numbers when reconnecting
discordClient :: Token -> TMChan Event -> WS.ClientApp ()
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
                atomically $ writeTMChan incoming event
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
