
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Discord.Client
    ( MonadDiscord(..)
    , Discord(..)
    , runDiscord
    , Env(..)
    , ReconnectPolicy(..)
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Monad.Reader
import qualified Network.WebSockets as WS
import           UnliftIO
import           UnliftIO.Concurrent
import           Wuss

import           Discord.Gateway

data Env = Env { incomingWS :: Chan GatewayMessage, outgoingWS :: Chan GatewayRequest }

data ReconnectPolicy =
    ReconnectAlways
  | ReconnectNever
    deriving Show

runDiscord :: MonadIO m => ReconnectPolicy -> Token -> Discord a -> m a
runDiscord policy token action = liftIO $ do
    incoming <- newChan
    outgoing <- newChan

    gtid <- runGatewayClient policy incoming outgoing

    runReaderT (unDiscord (login token >> action)) (Env incoming outgoing) `finally` killThread gtid

login :: Token -> Discord ()
login token = do
    hello <- receive
    case hello of
        Hello interval -> do
            -- _ <- spawnHeartbeatThread interval
            send (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs"))
            ready <- receive -- TODO: ignore heartbeat ack and start heartbeat thread before this?
            case ready of
                Dispatch _ (Ready _ _ _) -> do -- TODO: cache fields
                    _ <- spawnHeartbeatThread interval
                    forever $ receive >>= liftIO . print
                _  -> error ("expected ready, got " <> show ready)
        _ -> error ("expected hello, got " <> show hello)

spawnHeartbeatThread :: Int -> Discord ThreadId
spawnHeartbeatThread ms = forkIO $ forever $ do
    send (OutgoingHeartbeat 0) -- TODO: replace 0 with sequence
    threadDelay (ms * 1000)


-- TODO: rate limits
runGatewayClient :: ReconnectPolicy -> Chan GatewayMessage -> Chan GatewayRequest -> IO ThreadId
runGatewayClient policy incoming outgoing = forkWithUnmask $ \restore ->
    forever $ do
        restore (runSecureClient "gateway.discord.gg" 443 "/" $ \conn -> race_ (readMessages conn) (writeMessages conn))
            `catch` \(e :: SomeException) -> do
                putStrLn ("Exception in gateway client: " <> show e)
                case policy of
                    ReconnectAlways -> pure ()
                    ReconnectNever  -> throwIO e

        putStrLn "Lost connection. Reconnecting in 5 seconds..."
        threadDelay 5_000_000

    where

    readMessages conn = forever $ do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> writeChan incoming msg
            Left err  -> throwString ("Error decoding message: " <> err) -- TODO: create exception type

    writeMessages conn = forever $ do
        msg <- readChan outgoing
        WS.sendTextData conn (encode msg)

newtype Discord a = Discord { unDiscord :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

instance MonadDiscord Discord where
    receive = liftIO . readChan =<< asks incomingWS
    send msg = do
        outgoing <- asks outgoingWS
        liftIO (writeChan outgoing msg)

class MonadDiscord m where
    receive :: m GatewayMessage
    send    :: GatewayRequest -> m ()
    --sendRest    :: Request a -> m a
