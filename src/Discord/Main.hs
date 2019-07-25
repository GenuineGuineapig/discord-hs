
{-# language ImpredicativeTypes #-}

module Discord.Main where

import           Control.Monad (forever, when)
import           Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import           Network.Socket (withSocketsDo)
import           Polysemy

import Discord
import Discord.Rest.Channel

{-
printEvents :: Event -> Discord ()
printEvents = liftIO . print

pongBot :: Event -> Discord ()
pongBot = \case
  -- if message is "ping", send message to channel saying "pong"
    MessageCreate msg -> when (messageContent msg == "ping") $ do
        createdMsg <- runRequest (createMessage (messageChannelId msg) (CreateMessageRequest "pong" Nothing Nothing Nothing))
        liftIO (print createdMsg)
        pure ()
    _ -> pure ()

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"
    runDiscord token $ startGateway ReconnectAlways printEvents
-}

printEvents :: (Member (Embed IO) r, Member Gateway r) => Sem r ()
printEvents = forever $ do
    event <- receiveEvent
    case event of
        Just ev -> embed (print ev)
        Nothing -> pure ()

pongBot :: (Member (Embed IO) r, Member Request r, Member Gateway r) => Sem r ()
pongBot = forever $ do
    event <- receiveEvent
    case event of
        Just (MessageCreate msg) -> when (messageContent msg == "ping") $ do
            createdMsg <- createMessage (messageChannelId msg) (CreateMessageRequest "pong" Nothing Nothing Nothing)
            embed @IO (print createdMsg)
        _ -> pure ()

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"

    runM . requestToIO token $
        createMessage (Snowflake 574418206144593921) (CreateMessageRequest "test" Nothing Nothing Nothing)
    --runIt <- nat runM .@! runRateLimiting .@! const (runGateway token)
    --let runIt' = runIt . runRequest token

    --runIt' printEvents
    undefined




  {-
testy1 :: IO Message
testy1 = do
    token <- Token <$> TIO.readFile "../discord.auth"
    runIt <- nat runM .@! runRateLimiting
    let runIt' = runIt . runRequest token
    runIt' test1

-}
