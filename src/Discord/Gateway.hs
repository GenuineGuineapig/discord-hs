
module Discord.Gateway
    ( GatewayRequest(..)
    , GatewayMessage(..)
    , Token(..)
    , ConnectionProps(..)
    , Event(..)
    )
    where

import Data.Aeson
import Data.Aeson.Types
import Data.String (IsString)
import Data.Text (Text)

import Discord.Types


-- Gateway requests

data GatewayRequest = Identify Token ConnectionProps {- TODO Compression, Large threshold, Shard, Presence -} 
                    | Resume -- TODO
                    | OutgoingHeartbeat Int
                    | RequestGuildMembers -- TODO
                    | UpdateVoiceState -- TODO
                    | UpdateStatus -- TODO
                    deriving Show

newtype Token = Token { unToken :: Text } deriving (IsString, ToJSON)

instance Show Token where
    show _ = "<secret token>"



data ConnectionProps = ConnectionProps { os :: Text, browser :: Text, device :: Text } deriving Show

gatewayReq :: Int -> Value -> Value
gatewayReq op val = object [ "op" .= op, "d" .= val ]

instance ToJSON ConnectionProps where
    toJSON p = object ["$os" .= os p, "$browser" .= browser p, "$device" .= device p]

instance ToJSON GatewayRequest where
    toJSON = \case
        OutgoingHeartbeat _  -> gatewayReq 1 $ object [] -- TODO: sequence
        Identify token props -> gatewayReq 2 $ object ["token" .= token, "properties" .= props]
        r -> error ("unimplemented gateway request" <> show r)


-- Gateway messages

data GatewayMessage = Dispatch Int Event
                    | IncomingHeartbeat -- TODO
                    | Reconnect -- TODO
                    | InvalidSession Bool -- whether it's resumable
                    | Hello Int {- TODO trace -}
                    | HeartbeatAck
                    deriving Show

newtype SessionId = SessionId { unSessionId :: Text } deriving (FromJSON, IsString, Show)

data Event =
    Ready User {- TODO private channels -} [UnavailableGuild] SessionId {- TODO trace and shard -}
  | Resumed

  | GuildCreate Guild
  | GuildUpdate Guild
  | GuildDelete UnavailableGuild
  | GuildBanAdd Snowflake User
  | GuildBanRemove Snowflake User
  | GuildEmojisUpdate Snowflake [Emoji]
  | GuildIntegrationsUpdate
  | GuildMemberAdd Snowflake GuildMember
  | GuildMemberRemove Snowflake User
  | GuildMemberUpdate Snowflake [Snowflake] {- roles -} User Text {- nick -}
  | GuildMembersChunk Snowflake [GuildMember]
  | GuildRoleCreate Snowflake Role
  | GuildRoleUpdate Snowflake Role
  | GuildRoleDelete Snowflake Snowflake {- guild_id, role_id -}

  | ChannelCreate Channel
  | ChannelUpdate Channel
  | ChannelDelete Channel
  | ChannelPinsUpdate (Maybe Snowflake) {- guild id -} Snowflake Text

  | MessageCreate Message
  | MessageUpdate -- TODO
  | MessageDelete Snowflake Snowflake (Maybe Snowflake) {- message, channel, guild -}
  | MessageDeleteBulk [Snowflake] Snowflake (Maybe Snowflake) {- messages, channel, guild -}
  | MessageReactionAdd Snowflake Snowflake Snowflake (Maybe Snowflake) {- user, channel, message, guild, TODO: PARTIAL Emoji FFS -}
  | MessageReactionRemove Snowflake Snowflake Snowflake (Maybe Snowflake) {- user, channel, message, guild, TODO: PARTIAL Emoji FFS -}
  | MessageReactionRemoveAll Snowflake Snowflake (Maybe Snowflake) {- channel, message, guild -}

  | PresenceUpdate
  | TypingStart Snowflake (Maybe Snowflake) Snowflake Int {- channel. guild, user, timestamp in seconds -}
  | UserUpdate User

  | VoiceStateUpdate VoiceState
  | VoiceServerUpdate Text Snowflake Text {- token, guild, voice server host -}

  | WebhooksUpdate Snowflake Snowflake {- guild, channel -}

    deriving Show

eventFromJSON :: String -> Value -> Parser Event
eventFromJSON = \case
    "READY"   -> withObject "Ready" $ \obj -> Ready <$> obj .: "user" <*> obj .: "guilds" <*> obj .: "session_id"
    "RESUMED" -> const (pure Resumed)

    "GUILD_CREATE"              -> fmap GuildCreate . parseJSON
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
    "MESSAGE_UPDATE"      -> const (pure MessageUpdate) -- TODO: this has a PARTIAL message..
    "MESSAGE_DELETE"      -> withObject "MessageDelete"         $ \obj -> MessageDelete         <$> obj .: "id"      <*> obj .: "channel_id" <*> obj .:? "guild_id"
    "MESSAGE_DELETE_BULK" -> withObject "MessageDeleteBulk"     $ \obj -> MessageDeleteBulk     <$> obj .: "ids"     <*> obj .: "channel_id" <*> obj .:? "guild_id"

    "MESSAGE_REACTION_ADD"        -> withObject "MessageReactionAdd"       $ \obj -> MessageReactionAdd       <$> obj .: "user_id" <*> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id" -- <*> obj .: "emoji" TODO PARTIAL EMOJI
    "MESSAGE_REACTION_REMOVE"     -> withObject "MessageReactionRemove"    $ \obj -> MessageReactionRemove    <$> obj .: "user_id" <*> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id" -- <*> obj .: "emoji" TODO PARTIAL EMOJI
    "MESSAGE_REACTION_REMOVE_ALL" -> withObject "MessageReactionRemoveAll" $ \obj -> MessageReactionRemoveAll <$> obj .: "channel_id" <*> obj .: "message_id" <*> obj .:? "guild_id"

    "PRESENCE_UPDATE" -> const (pure PresenceUpdate) -- TODO
    "TYPING_START"    -> withObject "TypingStart" $ \obj -> TypingStart <$> obj .: "channel_id" <*> obj .:? "guild_id" <*> obj .: "user_id" <*> obj .: "timestamp"
    "USER_UPDATE"     -> fmap UserUpdate . parseJSON

    "VOICE_STATE_UPDATE" -> fmap VoiceStateUpdate . parseJSON
    "VOICE_SERVER_UPDATE" -> withObject "VoiceServerUpdate" $ \obj -> VoiceServerUpdate <$> obj .: "token" <*> obj .: "guild_id" <*> obj .: "endpoint"

    "WEBHOOKS_UPDATE" -> withObject "WebhooksUpdate" $ \obj -> WebhooksUpdate <$> obj .: "guild_id" <*> obj .: "channel_id"
    t -> error ("unimplemented event type: " <> t)


instance FromJSON GatewayMessage where
    parseJSON = withObject "Payload" $ \obj -> do
        op      <- obj .: "op" -- :: Parser Int TODO why isn't Parser in scope
        rawData <- obj .: "d"
        case op of
            0  -> do
                eventType <- obj .: "t"
                Dispatch <$> obj .: "s" <*> eventFromJSON eventType rawData
            1  -> fail ("Heartbeat unimplemented")
            7  -> fail ("Reconnect unimplemented")
            9  -> withBool "InvalidSession" (pure . InvalidSession) rawData
            10 -> withObject "Hello" (\data' -> Hello <$> data' .: "heartbeat_interval") rawData
            11 -> pure HeartbeatAck
            _  -> fail ("unknown opcode " <> show (op :: Int)) -- todo why isn't Parser in scope
