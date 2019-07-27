
{-# language ImpredicativeTypes #-}
{-# language TemplateHaskell    #-}

module Discord.Gateway
    ( Gateway
    , Handle(..)
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

import Discord.Gateway.Internal
import Discord.Types.Common
import Discord.Types.Gateway


data Gateway m a where
    ReceiveEvent :: Gateway m (Maybe Event)

makeSem ''Gateway

gatewayToInput :: Member (Input (Maybe Event)) r => Sem (Gateway ': r) a -> Sem r a
gatewayToInput = interpret (\ReceiveEvent -> input)

withGateway :: (Member (Embed IO) r, Member Async r, Member Resource r) => Token -> Sem (Gateway ': r) a -> Sem r a
withGateway token act =
    bracket (openGateway token)
            closeGateway
            (\(Handle _ incoming) -> inputFromChan incoming act)

inputFromChan :: Member (Embed IO) r => TMChan Event -> Sem (Gateway ': r) a -> Sem r a
inputFromChan incoming = runInputSem (embed @IO (atomically (readTMChan incoming))) . reinterpret (\ReceiveEvent -> input)

data Handle = Handle (A.Async (Maybe ())) (TMChan Event)

openGateway :: (Member (Embed IO) r, Member Async r, Member Resource r) => Token -> Sem r Handle
openGateway token = do
    incoming <- embed $ newTMChanIO
    task <- async $ runWsClient token incoming
    pure (Handle task incoming)

closeGateway :: MonadIO m => Handle -> m ()
closeGateway (Handle task events) = liftIO $ A.cancel task *> atomically (closeTMChan events)

runWsClient :: (Member (Embed IO) r, Member Resource r) => Token -> TMChan Event -> Sem r ()
runWsClient token incoming = do
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

readMessage :: WS.ClientApp GatewayMessage
readMessage conn = do
        rawMsg <- WS.receiveData conn :: IO BL.ByteString
        case eitherDecode rawMsg of
            Right msg -> pure msg
            Left err  -> E.throwIO $ DecodeException ("Error decoding message: " <> err)

writeMessage :: GatewayRequest -> WS.ClientApp ()
writeMessage msg conn = WS.sendTextData conn (encode msg)

-- why isn't this in Control.Exception
isSyncException :: E.Exception e => e -> Bool
isSyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> False
        Nothing -> True
