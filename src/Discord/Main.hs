
module Discord.Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text.IO as TIO
import           Network.HTTP.Req
import           Network.Socket (withSocketsDo)

import Discord.Client
import Discord.Gateway


baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"

runReq' :: FromJSON a => Req (JsonResponse a) -> IO a
runReq' = runReq defaultHttpConfig . fmap responseBody

appMain :: IO ()
appMain = undefined

printEvents :: Handler
printEvents = liftIO . print

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"
    startDiscord ReconnectAlways token printEvents

    --gatewayUrl <- runReq' $ req GET (baseUrl /: "gateway") NoReqBody jsonResponse mempty

    undefined
