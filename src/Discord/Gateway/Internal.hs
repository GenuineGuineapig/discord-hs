
{-# language ImpredicativeTypes #-}
{-# language TemplateHaskell    #-}

module Discord.Gateway.Internal
    ( DiscordException(..)
    , Session(..)
    , discordInSem
    , eventLoop
    , handshake
    )
    where

import           Data.Typeable (Typeable)
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import qualified Control.Exception as E
import           Control.Monad.Reader hiding (Reader)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Resource
import           Polysemy.State
import           Polysemy.Trace

import Discord.Types.Common
import Discord.Types.Gateway


data Session = Session
    { sessionId  :: SessionId
    , sessionSeq :: Int
    } deriving Show

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


data DiscordException =
    DecodeException String
  | ExpectedButFound String GatewayMessage
  | InvalidSessionException
  | ReconnectException -- discord told us to reconnect
  | UnexpectedHelloException -- discord sent a hello after the handshake
    deriving (Show, Typeable)

instance E.Exception DiscordException
