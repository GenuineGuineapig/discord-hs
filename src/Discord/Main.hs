
module Discord.Main where

import           Control.Monad (when)
import           Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import           Network.Socket (withSocketsDo)

import Discord.Client
import Discord.Gateway
import Discord.Rest
import Discord.Types


printEvents :: Handler
printEvents = liftIO . print

pongBot :: Handler
pongBot = \case
  -- if message is "ping", send message to channel saying "pong"
    MessageCreate msg -> when (messageContent msg == "ping") $ do
        createdMsg <- makeRequest (createMessage (messageChannelId msg) (CreateMessageRequest "pong" Nothing Nothing Nothing))
        liftIO (print createdMsg)
        pure ()
    _ -> pure ()

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"
    startDiscord ReconnectAlways token pongBot
