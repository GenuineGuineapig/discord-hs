
{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Discord.Main where

import           Control.Concurrent (threadDelay)
--import           Control.Monad (forever, unless)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.HTTP.Req
import           Network.Socket (withSocketsDo)
--import qualified Network.WebSockets as WS
--import           Wuss

import Discord.Client
import Discord.Gateway


baseUrl :: Url 'Https
baseUrl = https "discordapp.com" /: "api" /: "v6"

runReq' :: FromJSON a => Req (JsonResponse a) -> IO a
runReq' = runReq defaultHttpConfig . fmap responseBody

appMain :: IO ()
appMain = undefined

main :: IO ()
main = withSocketsDo $ do
    token <- Token <$> TIO.readFile "../discord.auth"
    runDiscord token $ liftIO (threadDelay maxBound)

    --gatewayUrl <- runReq' $ req GET (baseUrl /: "gateway") NoReqBody jsonResponse mempty

    undefined
