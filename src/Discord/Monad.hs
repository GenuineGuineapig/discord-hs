
module Discord.Monad
    ( Discord(..)
    , Env(..)
    , runDiscord
    , runRequest
    )
    where

import Control.Lens
import Control.Monad.Reader

import Discord.Gateway
import Discord.Rest
import UnliftIO hiding (Handler)

import Discord.Types.Common
import Discord.Types.Gateway


data Env = Env { envToken      :: Token
               , envRateLimits :: RateLimits
               , envIncoming   :: Chan Event
               }

instance HasToken Env where
    tokenL = lens envToken (\x y -> x { envToken = y })

instance HasRateLimits Env where
    rateLimitsL = lens envRateLimits (\x y -> x { envRateLimits = y })

instance HasIncomingEvents Env where
    incomingEventsL = lens envIncoming (\x y -> x { envIncoming = y })

newtype Discord a = Discord { unDiscord :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runDiscord :: Token -> Discord a -> IO a
runDiscord token action = do
    rateLimits <- newRateLimits
    incoming   <- newChan

    runReaderT (unDiscord action) (Env token rateLimits incoming)
