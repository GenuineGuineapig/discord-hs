
module GatewaySpec where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace

import Test.Hspec

import Discord.Gateway.Internal
import Discord.Types.Gateway

spec :: Spec
spec = do
    let session = Session "sessionid" 0
        token   = "token"

    describe "eventLoop" $ do
        let runIt incoming = run
                  . runState (Just session)
                  . runError @DiscordException
                  . runInputStream @GatewayMessage incoming
                  . runOutputList @Event
                  . ignoreTrace
                  $ eventLoop (sessionId session)

        it "should maintain a session" $ do
            let (session', _) = runIt [InvalidSession True]

            session' `shouldSatisfy` \case
                Just _  -> True
                Nothing -> False

        it "should invalidate a session" $ do
            let (session', _) = runIt [InvalidSession False]

            session' `shouldBe` Nothing

        it "should update the sequence number for each dispatch" $ do
            let (session', _) = runIt [Dispatch 1 Resumed, Reconnect]
            session' `shouldSatisfy` \case
                Just (Session{sessionSeq=1}) -> True
                _ -> False

    describe "handshake" $ do
        it "should attempt to resume a session" $ do
            let runIt = run
                      . runInputStream @GatewayMessage [Hello 5]
                      . runOutputList @GatewayRequest
                      . runError @DiscordException

                (reqs, _) = runIt (handshake (Just session) token)

            -- It should only send a resume request
            reqs `shouldSatisfy` \case
                [Resume{}] -> True
                _ -> False

runInputStream :: [i] -> Sem (Input i ': r) a -> Sem r a
runInputStream xs = evalState xs . reinterpret (\case
    Input -> get >>= \case
        (y:ys) -> put ys *> pure y
        _      -> error "EOF")
