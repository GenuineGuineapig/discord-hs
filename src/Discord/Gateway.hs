
{-# language ImpredicativeTypes #-}
{-# language TemplateHaskell    #-}

module Discord.Gateway
    ( Gateway
    , closeGateway
    , gatewayToInput
    , openGateway
    , receiveEvent
    , withGateway
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM.TMChan
import           Control.Exception hiding (bracket, finally)
import           Control.Monad.Reader hiding (Reader)
import           Control.Monad.STM
import           Data.IORef
import qualified Network.WebSockets as WS
import           Polysemy
import           Polysemy.Async
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Resource
import           Polysemy.State
import           Polysemy.Trace
import           Wuss

import Discord.Types.Common
import Discord.Types.Gateway


data Gateway m a where
    ReceiveEvent :: Gateway m (Maybe Event)

makeSem ''Gateway

gatewayToInput :: Member (Input (Maybe Event)) r => Sem (Gateway ': r) a -> Sem r a
gatewayToInput = interpret (\ReceiveEvent -> input)

runGatewayChan :: Member (Embed IO) r => TMChan Event -> Sem (Gateway ': r) a -> Sem r a
runGatewayChan incoming = runInputSem (embed @IO (atomically (readTMChan incoming))) . reinterpret (\ReceiveEvent -> input)

data Handle = Handle (A.Async (Maybe ())) (TMChan Event)

openGateway :: (Member (Embed IO) r, Member Async r, Member Resource r) => Token -> Sem r Handle
openGateway token = do
    incoming <- embed $ newTMChanIO
    task <- async $ runGatewayClient token incoming
    pure (Handle task incoming)

closeGateway :: MonadIO m => Handle -> m ()
closeGateway (Handle task events) = liftIO $ A.cancel task *> atomically (closeTMChan events)

runGatewayClient :: (Member (Embed IO) r, Member Resource r) => Token -> TMChan Event -> Sem r ()
runGatewayClient token incoming = do
    flip finally (embed $ atomically (closeTMChan incoming)) $ do
        sequenceRef <- embed $ newIORef Nothing

        forever $ do
            embed $ runSecureClient "gateway.discord.gg" 443 "/" (discordClient token incoming sequenceRef)
                `catch` \(e :: SomeException) ->
                    if isSyncException e
                      then putStrLn ("Exception in gateway client: " <> show e)
                      else throw e

            -- TODO: trace?
            embed $ putStrLn "Lost connection. Reconnecting in 5 seconds..."
            embed $ threadDelay 5_000_000

withGateway :: (Member (Embed IO) r, Member Async r, Member Resource r) => Token -> Sem (Gateway ': r) a -> Sem r a
withGateway token act =
    bracket (openGateway token)
            closeGateway
            (\(Handle _ incoming) -> runGatewayChan incoming act)


-- TODO: add more info in the IORef: reconnect token, etc
discordClient :: Token -> TMChan Event -> IORef (Maybe Int) -> WS.ClientApp ()
discordClient token incoming sequenceRef conn = runIt (discordInSem token)

    where

    outputToWs :: Member (Embed IO) r => Sem (Output GatewayRequest ': r) a -> Sem r a
    outputToWs = interpret (\(Output a) -> embed $ writeMessage a conn)

    outputToChan :: Member (Embed IO) r => Sem (Output Event ': r) a -> Sem r a
    outputToChan = interpret (\(Output a) -> embed $ atomically (writeTMChan incoming a))

    runIt = (runM .@ lowerResource .@ lowerAsync)
          . runStateIORef sequenceRef
          . outputToChan
          . outputToWs
          . runInputSem (embed $ readMessage conn)
          . traceToIO

discordInSem :: Members
             '[ Embed IO
              , Async
              , Resource
              , Input GatewayMessage -- incoming messages
              , Output Event -- events to pass to user code
              , Output GatewayRequest -- outgoing messages
              , State (Maybe Int) -- sequence number
              , Trace
              ] r
             => Token -> Sem r ()
discordInSem token = do
    heartbeatInterval <- login token

    task <- async (heartbeat heartbeatInterval)

    flip finally (embed (A.cancel task)) $ forever $ do
        msg <- input
        case msg of
            HeartbeatAck -> pure ()
            Dispatch n event -> do
                put (Just n)
                output @Event event
            t -> trace ("UNHANDLED: " <> show t)

    where

    heartbeat :: Members
              '[ Embed IO
               , Output GatewayRequest
               , State (Maybe Int) -- sequence number
               ] r
              => Int -> Sem r ()
    heartbeat interval = forever $ do
        currentSeq <- get
        output (OutgoingHeartbeat currentSeq)
        -- ew, embed.
        embed $ threadDelay (interval * 1000)

login :: Members '[Input GatewayMessage, Output GatewayRequest] r
      => Token -> Sem r Int
login token = do
    heartbeatInterval <- receiveHello
    output (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs") Nothing Nothing Nothing Nothing)
    _ <- receiveReady -- TODO: unpack cache values?
    pure heartbeatInterval

receiveHello :: Member (Input GatewayMessage) r => Sem r Int
receiveHello = do
    msg <- input
    case msg of
        Hello interval -> pure interval
        _ -> receiveHello -- TODO: exception? unexpected message

receiveReady :: Member (Input GatewayMessage) r => Sem r Event
receiveReady = do
    msg <- input
    case msg of
        Dispatch _ event@Ready{} -> pure event
        _ -> receiveReady -- TODO: exception? unexpected message

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> throwIO $ DecodeException ("Error decoding message: " <> err)

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

data DiscordException = DecodeException String deriving Show

instance Exception DiscordException

-- why isn't this in Control.Exception
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True
