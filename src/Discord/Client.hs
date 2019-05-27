
{-# language OverloadedStrings #-}

module Discord.Client
    ( MonadDiscord(..)
    , Discord(..)
    , runDiscord
    , Env(..)
    )
    where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import           UnliftIO
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           Wuss

import           Discord.Gateway

data Env = Env { incomingWS :: Chan GatewayMessage, outgoingWS :: Chan GatewayRequest }


runDiscord :: MonadIO m => Token -> Discord a -> m a
runDiscord token action = liftIO $ do
    incoming <- newChan
    outgoing <- newChan

    forkIO $ runGatewayClient incoming outgoing

    runReaderT (unDiscord (login token >> action)) (Env incoming outgoing)

login :: Token -> Discord ()
login token = do
    hello <- receive
    case hello of
        Hello interval -> do
            -- _ <- spawnHeartbeatThread interval
            send (Identify token (ConnectionProps "linux" "discord-hs" "discord-hs"))
            ready <- receive -- TODO: ignore heartbeat ack and start heartbeat thread before this?
            case ready of
                Dispatch s (Ready user unavailableGuilds sessionId) -> do
                    _ <- spawnHeartbeatThread interval
                    forever $ receive >>= liftIO . print
                _  -> error ("expected ready, got " <> show ready)
        _ -> error ("expected hello, got " <> show hello)

spawnHeartbeatThread :: Int -> Discord ThreadId
spawnHeartbeatThread ms = forkIO $ forever $ do
    send (OutgoingHeartbeat 0) -- TODO: replace 0 with sequence
    threadDelay (ms * 1000)


-- TODO: rate limits
runGatewayClient :: Chan GatewayMessage -> Chan GatewayRequest -> IO ()
runGatewayClient incoming outgoing = runSecureClient "gateway.discord.gg" 443 "/" $ \conn -> do
    _ <- forkIO $ forever $ do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        --print rawMsg
        case eitherDecode rawMsg of
            Right msg -> writeChan incoming msg
            Left err  -> error err

    forever $ do
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
