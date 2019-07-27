
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
import           Data.Typeable (Typeable)
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception as E
import           Control.Monad.Reader hiding (Reader)
import           Control.Monad.STM
import           Data.IORef
import qualified Network.WebSockets as WS
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
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
        sessionRef <- embed $ newIORef Nothing

        forever $ do
            embed $ runSecureClient "gateway.discord.gg" 443 "/" (discordClient token incoming sessionRef)
                `E.catch` \(e :: E.SomeException) ->
                    if isSyncException e
                      then putStrLn ("Exception in gateway client: " <> show e)
                      else E.throw e

            -- TODO: trace?
            embed $ putStrLn "Lost connection. Reconnecting in 5 seconds..."
            embed $ threadDelay 5_000_000

withGateway :: (Member (Embed IO) r, Member Async r, Member Resource r) => Token -> Sem (Gateway ': r) a -> Sem r a
withGateway token act =
    bracket (openGateway token)
            closeGateway
            (\(Handle _ incoming) -> runGatewayChan incoming act)


discordClient :: Token -> TMChan Event -> IORef (Maybe Session) -> WS.ClientApp ()
discordClient token incoming sessionRef conn = do
    runIt (discordInSem token) >>= either E.throwIO pure

    where

    outputToWs :: Member (Embed IO) r => Sem (Output GatewayRequest ': r) a -> Sem r a
    outputToWs = interpret (\(Output a) -> embed $ writeMessage a conn)

    outputToChan :: Member (Embed IO) r => Sem (Output Event ': r) a -> Sem r a
    outputToChan = interpret (\(Output a) -> embed $ atomically (writeTMChan incoming a))

    runIt = (runM .@ lowerResource .@ lowerAsync .@@ lowerError @DiscordException)
          . runStateIORef sessionRef
          . outputToChan
          . outputToWs
          . runInputSem (embed $ readMessage conn)
          . traceToIO

discordInSem :: Members
             '[ Embed IO
              , Async
              , Error DiscordException
              , Resource
              , Input GatewayMessage -- incoming messages
              , Output Event -- events to pass to user code
              , Output GatewayRequest -- outgoing messages
              , State (Maybe Session)
              , Trace
              ] r
             => Token -> Sem r ()
discordInSem token = do
    currentSession <- get

    (heartbeatInterval, sid) <- handshake currentSession token

    task <- async (heartbeat heartbeatInterval)

    eventLoop sid `finally` embed (A.cancel task)

    where

    heartbeat :: Members
              '[ Embed IO
               , Output GatewayRequest
               , State (Maybe Session) -- sequence number
               ] r
              => Int -> Sem r ()
    heartbeat interval = forever $ do
        session <- get
        output (OutgoingHeartbeat (sessionSeq <$> session))
        -- ew, embed.
        embed $ threadDelay (interval * 1000)

eventLoop :: Members
          '[ Error DiscordException
           , Input GatewayMessage
           , Output Event
           , Output GatewayRequest
           , State (Maybe Session)
           , Trace
           ] r
          => SessionId -> Sem r ()
eventLoop sid = forever $ do
    msg <- input
    case msg of
        HeartbeatAck -> pure ()
        IncomingHeartbeat -> pure ()

        Dispatch n event -> do
            put @(Maybe Session) (Just (Session sid n))
            output @Event event

        Hello _ -> throw UnexpectedHelloException
        InvalidSession resumable -> do
            when (not resumable) (put @(Maybe Session) Nothing)
            throw InvalidSessionException
        Reconnect -> throw ReconnectException

data Session = Session
    { sessionId  :: SessionId
    , sessionSeq :: Int
    } deriving Show

handshake :: Members
          '[ Error DiscordException
           , Input GatewayMessage
           , Output GatewayRequest
           ] r
          => Maybe Session -> Token -> Sem r (Int, SessionId)
handshake currentSession token = do
    msg <- input

    interval <- case msg of
        Hello heartbeatInterval -> pure heartbeatInterval
        ev -> throw (ExpectedButFound "Hello" ev)

    session <- case currentSession of
        Just session -> resume token session *> pure (sessionId session)
        _ -> login token

    pure (interval, session)

resume :: Member (Output GatewayRequest) r => Token -> Session -> Sem r ()
resume token session =
    output (Resume token (sessionId session) (sessionSeq session))

login :: Members
      '[ Error DiscordException
       , Input GatewayMessage
       , Output GatewayRequest
       ] r
      => Token -> Sem r SessionId
login token = do
    output (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs") Nothing Nothing Nothing Nothing)

    input >>= \case
        Dispatch _ (Ready _ _ _ sid _) -> pure sid
        ev -> throw (ExpectedButFound "Ready" ev)

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> E.throwIO $ DecodeException ("Error decoding message: " <> err)

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

data DiscordException =
    DecodeException String
  | ExpectedButFound String GatewayMessage
  | InvalidSessionException
  | ReconnectException -- discord told us to reconnect
  | UnexpectedHelloException -- discord sent a hello after the handshake
    deriving (Show, Typeable)

instance E.Exception DiscordException

-- why isn't this in Control.Exception
isSyncException :: E.Exception e => e -> Bool
isSyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> False
        Nothing -> True
