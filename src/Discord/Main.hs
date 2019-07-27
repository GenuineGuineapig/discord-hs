
{-# language ImpredicativeTypes #-}

module Discord.Main where

import           Control.Monad (forever, when)
import qualified Data.Text.IO as TIO
import           Network.Socket (withSocketsDo)
import           Polysemy
import           Polysemy.Async
import           Polysemy.Resource
import           Polysemy.Trace

import Discord
import Discord.Rest.Channel

traceEvents :: (Member Trace r, Member Gateway r) => Sem r ()
traceEvents = forever $ do
    event <- receiveEvent
    case event of
        Just ev -> trace (show ev)
        Nothing -> pure ()

pongBot :: (Member Trace r, Member Request r, Member Gateway r) => Sem r ()
pongBot = forever $ do
    event <- receiveEvent
    case event of
        Just (MessageCreate msg) -> when (messageContent msg == "ping") $ do
            createdMsg <- createMessage (messageChannelId msg) (CreateMessageRequest "pong" Nothing Nothing Nothing)
            trace (show createdMsg)
        _ -> pure ()

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"

    let runIt = (runM .@ lowerResource .@ lowerAsync)
              . traceToIO
              . requestToIO token
              . withGateway token

    runIt $ do
        trace "Connecting..."
        traceEvents
