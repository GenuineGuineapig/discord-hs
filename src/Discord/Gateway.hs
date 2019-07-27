
{-# language TemplateHaskell    #-}

module Discord.Gateway
    ( Gateway
    , Handle(..)
    , closeGateway
    , openGateway
    , receiveEvent
    , withGateway
    )
    where

import           Data.Aeson
import qualified Data.ByteString as BS
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
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

import Discord.Gateway.Internal
import Discord.Types.Common
import Discord.Types.Gateway


-- An effect for receiving events from the Discord Gateway
-- receiveEvent will produce Nothing after the gateway terminates
data Gateway m a where
    ReceiveEvent :: Gateway m (Maybe Event)

makeSem ''Gateway

-- Spawn a gateway client thread and use it to interpret the Gateway effect
withGateway :: forall r a.
               Members
            '[ Embed IO
             , Async
             , Resource
             ] r
            => Token -> Sem (Gateway ': r) a -> Sem r a
withGateway token act = bracket
    (openGateway token)
    closeGateway
    (\(Handle _ incoming) -> inputFromChan incoming act)

    where

    inputFromChan :: TMChan Event -> Sem (Gateway ': r) a -> Sem r a
    inputFromChan incoming =
        interpret (\ReceiveEvent -> embed (atomically (readTMChan incoming)))

---------- Raw gateway operations

data Handle = Handle (A.Async (Maybe ())) -- client thread
                     (TMChan Event) -- incoming events

openGateway :: Members
            '[ Embed IO
             , Async
             , Resource
             ] r
            => Token -> Sem r Handle
openGateway token = do
    incoming <- embed $ newTMChanIO
    task <- async $ runClientApp token incoming
    pure (Handle task incoming)

closeGateway :: MonadIO m => Handle -> m ()
closeGateway (Handle task events) =
    liftIO $ A.cancel task *> atomically (closeTMChan events)

runClientApp :: Members
             '[ Embed IO
              , Resource
              ] r
             => Token -> TMChan Event -> Sem r ()
runClientApp token incoming = do
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

---------- Wrap discord Sem client into a websocket app

discordClient :: Token
              -> TMChan Event -- incoming events chan
              -> IORef (Maybe Session) -- persistent session state for reconnections
              -> WS.ClientApp ()
discordClient token incoming sessionRef conn = do
    runIt (discordInSem token) >>= either E.throwIO pure

    where

    runIt = (runM .@ lowerResource .@ lowerAsync .@@ lowerError @DiscordException)
          . runStateIORef sessionRef
          . outputEvents
          . outputToWs
          . inputFromWs
          . traceToIO

    -- Input GatewayMessage, Output GatewayRequest, Output Event
    inputFromWs  = runInputSem  $       embed (readMessage conn)
    outputToWs   = runOutputSem $ \a -> embed (writeMessage a conn)
    outputEvents = runOutputSem $ \a -> embed (atomically (writeTMChan incoming a))

    runOutputSem :: (o -> Sem r ()) -> Sem (Output o ': r) a -> Sem r a
    runOutputSem act = interpret (\(Output o) -> act o)

readMessage :: WS.Connection -> IO GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BS.ByteString
        case eitherDecodeStrict rawMsg of
            Right msg -> pure msg
            Left err  -> E.throwIO $ DecodeException ("Error decoding message: " <> err)

writeMessage :: GatewayRequest -> WS.Connection -> IO ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

-- why isn't this in Control.Exception
isSyncException :: E.Exception e => e -> Bool
isSyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> False
        Nothing -> True
