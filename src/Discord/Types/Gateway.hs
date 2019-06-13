
module Discord.Types.Gateway
    ( ConnectionProps(..)
    , Event(..)
    , GatewayRequest(..)
    , GatewayMessage(..)
    , ReconnectPolicy(..)
    , Token(..)
    )
    where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T

import Discord.Types.Common


data ReconnectPolicy =
    ReconnectAlways
  | ReconnectNever
    deriving Show

-- Gateway requests

data GatewayRequest = Identify Token ConnectionProps (Maybe Bool) (Maybe Int) (Maybe [Int]) (Maybe Presence) {- token, props, compress, large threshold, shard, presence -}
                    | Resume Token Text Int {- token, session ID, last sequence number received-}
                    | OutgoingHeartbeat (Maybe Int)
                    | RequestGuildMembers [Snowflake Guild] Text Int {- guilds, query (prefix), limit -}
                    | UpdateVoiceState (Snowflake Guild) (Maybe (Snowflake Channel)) Bool Bool {- guild, channel, mute, deaf -}
                    | UpdateStatus Presence
                    deriving Show



data ConnectionProps = ConnectionProps { os :: Text, browser :: Text, device :: Text } deriving Show

gatewayReq :: Int -> Value -> Value
gatewayReq op val = object [ "op" .= op, "d" .= val ]

instance ToJSON ConnectionProps where
    toJSON p = object ["$os" .= os p, "$browser" .= browser p, "$device" .= device p]

instance ToJSON GatewayRequest where
    toJSON = \case
        OutgoingHeartbeat s                                -> gatewayReq 1 $ maybe Null (Number . fromIntegral) s
        Identify token props compress large shard presence -> gatewayReq 2 $ object $ filter (\(_,v) -> v /= Null) ["token" .= token, "properties" .= props, "compress" .= compress, "large_threshold" .= large, "shard" .= shard, "presence" .= presence]
        UpdateStatus presence                              -> gatewayReq 3 $ toJSON presence
        UpdateVoiceState guild channel mute deaf           -> gatewayReq 4 $ object ["guild_id" .= guild, "channel_id" .= channel, "self_mute" .= mute, "self_deaf" .= deaf]
        Resume token sessionId msgId                       -> gatewayReq 6 $ object ["token" .= token, "session_id" .= sessionId, "seq" .= msgId]
        RequestGuildMembers guild query limit              -> gatewayReq 8 $ object ["guild_id" .= guild, "query" .= query, "limit" .= limit]


-- Gateway messages

data GatewayMessage = Dispatch Int Event
                    | IncomingHeartbeat
                    | Reconnect
                    | InvalidSession Bool -- whether it's resumable
                    | Hello Int
                    | HeartbeatAck
                    deriving Show

newtype SessionId = SessionId { unSessionId :: Text } deriving (FromJSON, IsString, Show)

data Event =
    Ready User [Channel] [UnavailableGuild] SessionId (Maybe [Int]) {- user, private channels, guilds, session, shard info -}
  | Resumed

  | GuildCreate Guild [GuildMember] [GuildPresence]
  | GuildUpdate Guild
  | GuildDelete UnavailableGuild
  | GuildBanAdd (Snowflake Guild) User
  | GuildBanRemove (Snowflake Guild) User
  | GuildEmojisUpdate (Snowflake Guild) [Emoji]
  | GuildIntegrationsUpdate
  | GuildMemberAdd (Snowflake Guild) GuildMember
  | GuildMemberRemove (Snowflake Guild) User
  | GuildMemberUpdate (Snowflake Guild) [Snowflake Role] User Text {- nick -}
  | GuildMembersChunk (Snowflake Guild) [GuildMember]
  | GuildRoleCreate (Snowflake Guild) Role
  | GuildRoleUpdate (Snowflake Guild) Role
  | GuildRoleDelete (Snowflake Guild) (Snowflake Role) {- guild_id, role_id -}

  | ChannelCreate Channel
  | ChannelUpdate Channel
  | ChannelDelete Channel
  | ChannelPinsUpdate (Maybe (Snowflake Guild)) (Snowflake Message) Text {- last pin timestamp -}

  | MessageCreate Message
  | MessageUpdate Message
  | MessageDelete (Snowflake Message) (Snowflake Channel) (Maybe (Snowflake Guild))
  | MessageDeleteBulk [Snowflake Message] (Snowflake Channel) (Maybe (Snowflake Guild))
  | MessageReactionAdd (Snowflake User) (Snowflake Channel) (Snowflake Message) (Maybe (Snowflake Guild)) Emoji
  | MessageReactionRemove (Snowflake User) (Snowflake Channel) (Snowflake Message) (Maybe (Snowflake Guild)) Emoji
  | MessageReactionRemoveAll (Snowflake Channel) (Snowflake Message) (Maybe (Snowflake Guild))

  | PresenceUpdate Value [Snowflake Role] (Maybe Activity) (Snowflake Guild) Status [Activity] ClientStatus {- user, roles, game, guild_id, status, activities, client_status -}
  | TypingStart (Snowflake Channel) (Maybe (Snowflake Guild)) (Snowflake User) Int {- channel. guild, user, timestamp in seconds -}
  | UserUpdate User

  | VoiceStateUpdate VoiceState
  | VoiceServerUpdate Text (Snowflake Guild) Text {- token, guild, voice server host -}

  | WebhooksUpdate (Snowflake Guild) (Snowflake Channel)

    deriving Show

eventFromJSON :: String -> Value -> Parser Event
eventFromJSON = \case
    "READY"   -> withObject "Ready" $ \obj -> Ready <$> obj .: "user" <*> obj .: "private_channels" <*> obj .: "guilds" <*> obj .: "session_id" <*> obj .:? "shard"
    "RESUMED" -> const (pure Resumed)

    "GUILD_CREATE"              -> withObject "GuildCreate" $ \obj -> GuildCreate <$> parseJSON (Object obj) <*> obj .: "members" <*> obj .: "presences"
    "GUILD_UPDATE"              -> fmap GuildUpdate . parseJSON
    "GUILD_DELETE"              -> fmap GuildDelete . parseJSON
    "GUILD_BAN_ADD"             -> withObject "GuildBanAdd"       $ \obj -> GuildBanAdd       <$> obj .: "guild_id" <*> obj .: "user"
    "GUILD_BAN_REMOVE"          -> withObject "GuildBanRemove"    $ \obj -> GuildBanRemove    <$> obj .: "guild_id" <*> obj .: "user"
    "GUILD_EMOJIS_UPDATE"       -> withObject "GuildEmojisUpdate" $ \obj -> GuildEmojisUpdate <$> obj .: "guild_id" <*> obj .: "emojis"
    "GUILD_INTEGRATIONS_UPDATE" -> const (pure GuildIntegrationsUpdate)
    "GUILD_MEMBER_ADD"          -> \v -> GuildMemberAdd <$> withObject "GuildMemberAdd" (.: "guild_id") v <*> parseJSON v -- discord idiocy.
    "GUILD_MEMBER_REMOVE"       -> withObject "GuildMemberRemove" $ \obj -> GuildMemberRemove <$> obj .: "guild_id" <*> obj .: "user"
    "GUILD_MEMBER_UPDATE"       -> withObject "GuildMemberUpdate" $ \obj -> GuildMemberUpdate <$> obj .: "guild_id" <*> obj .: "roles" <*> obj .: "user" <*> obj .: "nick"
    "GUILD_MEMBERS_CHUNK" -> withObject "GuildMembersChunk" $ \obj -> GuildMembersChunk <$> obj .: "guild_id" <*> obj .: "members"
    "GUILD_ROLE_CREATE"   -> withObject "GuildRoleCreate"   $ \obj -> GuildRoleCreate   <$> obj .: "guild_id" <*> obj .: "role"
    "GUILD_ROLE_UPDATE"   -> withObject "GuildRoleUpdate"   $ \obj -> GuildRoleUpdate   <$> obj .: "guild_id" <*> obj .: "role"
    "GUILD_ROLE_DELETE"   -> withObject "GuildRoleDelete"   $ \obj -> GuildRoleDelete   <$> obj .: "guild_id" <*> obj .: "role_id"

    "CHANNEL_CREATE"      -> fmap ChannelCreate . parseJSON
    "CHANNEL_UPDATE"      -> fmap ChannelUpdate . parseJSON
    "CHANNEL_DELETE"      -> fmap ChannelDelete . parseJSON
    "CHANNEL_PINS_UPDATE" -> withObject "ChannelPinsUpdate" $ \obj -> ChannelPinsUpdate <$> obj .:? "guild_id" <*> obj .: "channel_id" <*> obj .: "timestamp"

    "MESSAGE_CREATE"      -> fmap MessageCreate . parseJSON
    "MESSAGE_UPDATE"      -> fmap MessageUpdate . parseJSON
    "MESSAGE_DELETE"      -> withObject "MessageDelete"         $ \obj -> MessageDelete         <$> obj .: "id"      <*> obj .: "channel_id" <*> obj .:? "guild_id"
    "MESSAGE_DELETE_BULK" -> withObject "MessageDeleteBulk"     $ \obj -> MessageDeleteBulk     <$> obj .: "ids"     <*> obj .: "channel_id" <*> obj .:? "guild_id"

    "MESSAGE_REACTION_ADD"        -> withObject "MessageReactionAdd"       $ \obj -> MessageReactionAdd       <$> obj .: "user_id" <*> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id" <*> obj .: "emoji"
    "MESSAGE_REACTION_REMOVE"     -> withObject "MessageReactionRemove"    $ \obj -> MessageReactionRemove    <$> obj .: "user_id" <*> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id" <*> obj .: "emoji"
    "MESSAGE_REACTION_REMOVE_ALL" -> withObject "MessageReactionRemoveAll" $ \obj -> MessageReactionRemoveAll <$> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id"

    "PRESENCE_UPDATE" -> withObject "PresenceUpdate" $ \obj -> PresenceUpdate <$> obj .: "user" <*> obj .: "roles" <*> obj .:? "game" <*> obj .: "guild_id" <*> obj .: "status" <*> obj .: "activities" <*> obj .: "client_status"
    "TYPING_START"    -> withObject "TypingStart" $ \obj -> TypingStart <$> obj .: "channel_id" <*> obj .:? "guild_id" <*> obj .: "user_id" <*> obj .: "timestamp"
    "USER_UPDATE"     -> fmap UserUpdate . parseJSON

    "VOICE_STATE_UPDATE" -> fmap VoiceStateUpdate . parseJSON
    "VOICE_SERVER_UPDATE" -> withObject "VoiceServerUpdate" $ \obj -> VoiceServerUpdate <$> obj .: "token" <*> obj .: "guild_id" <*> obj .: "endpoint"

    "WEBHOOKS_UPDATE" -> withObject "WebhooksUpdate" $ \obj -> WebhooksUpdate <$> obj .: "guild_id" <*> obj .: "channel_id"
    t -> error ("unimplemented event type: " <> t)

instance FromJSON GatewayMessage where
    parseJSON = withObject "Payload" $ \obj -> do
        op      <- obj .: "op" :: Parser Int
        rawData <- obj .: "d"
        case op of
            0  -> do
                eventType <- obj .: "t"
                Dispatch <$> obj .: "s" <*> eventFromJSON eventType rawData
            1  -> pure IncomingHeartbeat
            7  -> pure Reconnect
            9  -> withBool "InvalidSession" (pure . InvalidSession) rawData
            10 -> withObject "Hello" (\data' -> Hello <$> data' .: "heartbeat_interval") rawData
            11 -> pure HeartbeatAck
            _  -> fail ("unknown opcode " <> show (op :: Int)) -- todo why isn't Parser in scope


data Status =
    Online
  | DoNotDisturb
  | Idle
  | Invisible
  | Offline
    deriving Show

instance FromJSON Status where
    parseJSON = withText "Status" $ \case
        "online"    -> pure Online
        "dnd"       -> pure DoNotDisturb
        "idle"      -> pure Idle
        "invisible" -> pure Invisible
        "offline"   -> pure Offline
        s           -> fail ("unknown status" <> T.unpack s)

data ClientStatus = ClientStatus
    { clientStatusDesktop :: Maybe Status
    , clientStatusMobile  :: Maybe Status
    , clientStatusWeb     :: Maybe Status
    } deriving Show

instance FromJSON ClientStatus where
    parseJSON = withObject "ClientStatus" $ \obj ->
        ClientStatus <$> obj .:? "desktop"
                     <*> obj .:? "mobile"
                     <*> obj .:? "web"

instance ToJSON Status where
    toJSON = String . \case
        Online       -> "online"
        DoNotDisturb -> "dnd"
        Idle         -> "idle"
        Invisible    -> "invisible"
        Offline      -> "offline"

data Presence = Presence
    { presenceSince  :: Maybe Int -- "since" epoch millis if idle
    , presenceGame   :: Maybe Activity
    , presenceStatus :: Status
    , presenceAfk    :: Bool
    } deriving Show

instance ToJSON Presence where
    toJSON p = object ["since" .= presenceSince p, "game" .= presenceGame p, "status" .= presenceStatus p, "afk" .= presenceAfk p]

data GuildPresence = GuildPresence
    { guildPresenceUser         :: Value
    , guildPresenceGame         :: Maybe Activity
    , guildPresenceStatus       :: Status
    , guildPresenceActivities   :: [Activity]
    , guildPresenceClientStatus :: ClientStatus
    } deriving Show

instance FromJSON GuildPresence where
    parseJSON = withObject "GuildPresence" $ \obj ->
        GuildPresence <$> obj .:  "user"
                      <*> obj .:? "game"
                      <*> obj .:  "status"
                      <*> obj .:  "activities"
                      <*> obj .:  "client_status"
