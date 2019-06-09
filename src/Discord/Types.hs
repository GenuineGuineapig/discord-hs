
module Discord.Types
    ( Emoji(..)
    , Embed(..)
    , Guild(..)
    , GuildMember(..)
    , Channel(..)
    , Message(..)
    , Role(..)
    , Snowflake(..)
    , Token(..)
    , UnavailableGuild(..)
    , User(..)
    , VoiceState(..)
    )
    where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.String (IsString)
import Data.Text (Text)
import Data.Word


newtype Token = Token { unToken :: Text } deriving (IsString, ToJSON)

instance Show Token where
    show _ = "<secret token>"


newtype Snowflake = Snowflake { unSnowflake :: Word64 } deriving (Eq, Ord, Show)

instance FromJSON Snowflake where
    parseJSON = fmap (Snowflake . read) . parseJSON

instance ToJSON Snowflake where
    toJSON (Snowflake v) = toJSON (show v)

data User = User -- TODO: remaining items
    { userId            :: Snowflake
    , userName          :: Text
    , userDiscriminator :: Text
    } deriving Show

instance FromJSON User where
    parseJSON = withObject "User" $ \obj -> do
        User <$> obj .: "id" <*> obj .: "username" <*> obj .: "discriminator"


data UnavailableGuild = UnavailableGuild Snowflake deriving Show -- TODO: track whether we were kicked ("unavailable" is unset)

instance FromJSON UnavailableGuild where
    parseJSON = withObject "UnavailableGuild" $ \obj -> UnavailableGuild <$> obj .: "id"


data Channel = Channel -- TODO: sum type
    { channelId            :: Snowflake
    , channelType          :: Int -- TODO: sum type
    , channelGuildId       :: Maybe Snowflake
    , channelPosition      :: Maybe Integer
    , channelName          :: Maybe String
    , channelTopic         :: Maybe String
    , channelNsfw          :: Maybe Bool
    , channelLastMessageId :: Maybe Snowflake
    , channelBitrate       :: Maybe Integer
    , channelUserLimit     :: Maybe Integer
    -- TODO: remaining
    -- , channelPermissionOverwrites :: Maybe [Overwrite]
    } deriving Show

instance FromJSON Channel where
    parseJSON = withObject  "Channel" $ \obj ->
        Channel <$> obj .:  "id"
                <*> obj .:  "type"
                <*> obj .:  "guild_id"
                <*> obj .:? "position"
                <*> obj .:? "name"
                <*> obj .:? "topic"
                <*> obj .:? "nsfw"
                <*> obj .:? "last_message_id"
                <*> obj .:? "bitrate"
                <*> obj .:? "user_limit"


data Guild = Guild
    { guildId                :: Snowflake
    , guildName              :: Text
    , guildIcon              :: Maybe Text
    , guildSplash            :: Maybe Text
    , guildOwner             :: Maybe Bool
    , guildOwnerId           :: Snowflake
    , guildPermissions       :: Maybe Int
    , guildRegion            :: String
    , guildAfkChannelId      :: Maybe Snowflake
    , guildAfkTimeout        :: Int
    , guildEmbedEnabled      :: Maybe Bool
    , guildEmbedChannelId    :: Maybe Bool
    } deriving Show

instance FromJSON Guild where
    parseJSON = withObject "Guild" $ \obj -> 
        Guild <$> obj .:  "id"
              <*> obj .:  "name"
              <*> obj .:  "icon"
              <*> obj .:  "splash"
              <*> obj .:? "owner"
              <*> obj .:  "owner_id"
              <*> obj .:? "permissions"
              <*> obj .:  "region"
              <*> obj .:  "afk_channel_id"
              <*> obj .:  "afk_timeout"
              <*> obj .:? "embed_enabled"
              <*> obj .:? "embed_channel_id"

data Role = Role deriving Show -- TODO

instance FromJSON Role where
    parseJSON = const (pure Role)

data Emoji = Emoji
    { emojiId            :: Maybe Snowflake
    , emojiName          :: String
    , emojiRoles         :: Maybe [Role]
    , emojiUser          :: Maybe User
    , emojiRequireColons :: Maybe Bool
    , emojiManaged       :: Maybe Bool
    , emojiAnimated      :: Maybe Bool
    } deriving Show

instance FromJSON Emoji where
    parseJSON = withObject "Emoji" $ \obj ->
        Emoji <$> obj .:? "id"
              <*> obj .:  "name"
              <*> obj .:? "roles"
              <*> obj .:? "user"
              <*> obj .:? "require_colons"
              <*> obj .:? "managed"
              <*> obj .:? "animated"

data GuildMember = GuildMember
    { guildMemberUser     :: User
    , guildMemberNick     :: Maybe Text
    , guildMemberRoles    :: [Snowflake]
    , guildMemberJoinedAt :: Text -- TODO: timestamp
    , guildMemberDeaf     :: Bool
    , guildMemberMute     :: Bool
    } deriving Show

instance FromJSON GuildMember where
    parseJSON = withObject "GuildMember" $ \obj ->
        GuildMember <$> obj .:  "user"
                    <*> obj .:? "nick"
                    <*> obj .:  "roles"
                    <*> obj .:  "joined_at"
                    <*> obj .:  "deaf"
                    <*> obj .:  "mute"

data WebhookUser = WebhookUser
    { webhookUserId     :: Snowflake
    , webhookUserName   :: Text
    , webhookUserAvatar :: Maybe Text
    } deriving Show


instance FromJSON WebhookUser where
    parseJSON = withObject "WebhookUser" $ \obj ->
        WebhookUser <$> obj .:  "id"
                    <*> obj .:  "username"
                    <*> obj .:? "avatar"

data Attachment = Attachment
    { attachmentId       :: Snowflake
    , attachmentFilename :: Text
    , attachmentSize     :: Int
    , attachmentUrl      :: Text
    , attachmentProxyUrl :: Text
    , attachmentHeight   :: Maybe Int
    , attachmentWidth    :: Maybe Int
    } deriving Show

instance FromJSON Attachment where
    parseJSON = withObject "Attachment" $ \obj ->
        Attachment <$> obj .:  "id"
                   <*> obj .:  "filename"
                   <*> obj .:  "size"
                   <*> obj .:  "url"
                   <*> obj .:  "proxy_url"
                   <*> obj .:? "height"
                   <*> obj .:? "width"

data Embed = Embed
    { embedTitle       :: Maybe Text
    , embedType        :: Maybe Text
    , embedDescription :: Maybe Text
    , embedUrl         :: Maybe Text
    , embedTimestamp   :: Maybe Text -- TODO: timestamp
    , embedColor       :: Maybe Int -- TODO: rgb?
    , embedFooter      :: Maybe EmbedFooter
    , embedImage       :: Maybe EmbedImage
    , embedThumbnail   :: Maybe EmbedThumbnail
    , embedVideo       :: Maybe EmbedVideo
    , embedProvider    :: Maybe EmbedProvider
    , embedAuthor      :: Maybe EmbedAuthor
    , embedFields      :: Maybe [EmbedField]
    } deriving Show

instance FromJSON Embed where
    parseJSON = withObject "Embed" $ \obj ->
        Embed <$> obj .:? "title"
              <*> obj .:? "type"
              <*> obj .:? "description"
              <*> obj .:? "url"
              <*> obj .:? "timestamp"
              <*> obj .:? "color"
              <*> obj .:? "footer"
              <*> obj .:? "image"
              <*> obj .:? "thumbnail"
              <*> obj .:? "video"
              <*> obj .:? "provider"
              <*> obj .:? "author"
              <*> obj .:? "fields"

instance ToJSON Embed where
    toJSON p = object [ "title"       .= embedTitle p
                      , "type"        .= embedType p
                      , "description" .= embedDescription p
                      , "url"         .= embedUrl p
                      , "timestamp"   .= embedTimestamp p
                      , "color"       .= embedColor p
                      , "footer"      .= embedFooter p
                      , "image"       .= embedImage p
                      , "thumbnail"   .= embedThumbnail p
                      , "video"       .= embedVideo p
                      , "provider"    .= embedProvider p
                      , "author"      .= embedAuthor p
                      , "fields"      .= embedFields p
                      ]

data EmbedFooter = EmbedFooter
    { embedFooterText         :: Text
    , embedFooterIconUrl      :: Maybe Text
    , embedFooterProxyIconUrl :: Maybe Text
    } deriving Show

instance FromJSON EmbedFooter where
    parseJSON = withObject "EmbedFooter" $ \obj ->
        EmbedFooter <$> obj .:  "text"
                    <*> obj .:? "icon_url"
                    <*> obj .:? "proxy_icon_url"

instance ToJSON EmbedFooter where
    toJSON p = object [ "text"           .= embedFooterText p
                      , "icon_url"       .= embedFooterIconUrl p
                      , "proxy_icon_url" .= embedFooterProxyIconUrl p
                      ]

data EmbedImage = EmbedImage
    { embedImageUrl      :: Maybe Text
    , embedImageProxyUrl :: Maybe Text
    , embedImageHeight   :: Maybe Int
    , embedImageWidth    :: Maybe Int
    } deriving Show

instance FromJSON EmbedImage where
    parseJSON = withObject "EmbedImage" $ \obj ->
        EmbedImage <$> obj .:? "url"
                   <*> obj .:? "proxy_url"
                   <*> obj .:? "height"
                   <*> obj .:? "width"

instance ToJSON EmbedImage where
    toJSON p = object [ "url"       .= embedImageUrl p
                      , "proxy_url" .= embedImageProxyUrl p
                      , "height"    .= embedImageHeight p
                      , "width"     .= embedImageWidth p
                      ]

data EmbedThumbnail = EmbedThumbnail
    { embedThumbnailUrl      :: Maybe Text
    , embedThumbnailProxyUrl :: Maybe Text
    , embedThumbnailHeight   :: Maybe Int
    , embedThumbnailWidth    :: Maybe Int
    } deriving Show

instance FromJSON EmbedThumbnail where
    parseJSON = withObject "EmbedThumbnail" $ \obj ->
        EmbedThumbnail <$> obj .:? "url"
                       <*> obj .:? "proxy_url"
                       <*> obj .:? "height"
                       <*> obj .:? "width"

instance ToJSON EmbedThumbnail where
    toJSON p = object [ "url"       .= embedThumbnailUrl p
                      , "proxy_url" .= embedThumbnailProxyUrl p
                      , "height"    .= embedThumbnailHeight p
                      , "width"     .= embedThumbnailWidth p
                      ]

data EmbedVideo = EmbedVideo
    { embedVideoUrl    :: Maybe Text
    , embedVideoHeight :: Maybe Int
    , embedVideoWidth  :: Maybe Int
    } deriving Show

instance FromJSON EmbedVideo where
    parseJSON = withObject "EmbedVideo" $ \obj ->
        EmbedVideo <$> obj .:? "url"
                   <*> obj .:? "height"
                   <*> obj .:? "width"

instance ToJSON EmbedVideo where
    toJSON p = object [ "url"    .= embedVideoUrl p
                      , "height" .= embedVideoHeight p
                      , "width"  .= embedVideoWidth p
                      ]

data EmbedProvider = EmbedProvider
    { embedProviderName :: Maybe Text
    , embedProviderUrl  :: Maybe Text
    } deriving Show

instance FromJSON EmbedProvider where
    parseJSON = withObject "EmbedProvider" $ \obj ->
        EmbedProvider <$> obj .:? "name"
                      <*> obj .:? "url"

instance ToJSON EmbedProvider where
    toJSON p = object [ "name" .= embedProviderName p
                      , "url"  .= embedProviderUrl p
                      ]

data EmbedAuthor = EmbedAuthor
    { embedAuthorName         :: Maybe Text
    , embedAuthorUrl          :: Maybe Text
    , embedAuthorIconUrl      :: Maybe Text
    , embedAuthorProxyIconUrl :: Maybe Text
    } deriving Show

instance FromJSON EmbedAuthor where
    parseJSON = withObject "EmbedAuthor" $ \obj ->
        EmbedAuthor <$> obj .:? "name"
                    <*> obj .:? "url"
                    <*> obj .:? "icon_url"
                    <*> obj .:? "proxy_icon_url"

instance ToJSON EmbedAuthor where
    toJSON p = object [ "name"           .= embedAuthorName p
                      , "url"            .= embedAuthorUrl p
                      , "icon_url"       .= embedAuthorIconUrl p
                      , "proxy_icon_url" .= embedAuthorProxyIconUrl p
                      ]

data EmbedField = EmbedField
    { embedFieldName :: Text
    , embedFieldValue :: Text
    , embedFieldInline :: Maybe Bool
    } deriving Show

instance FromJSON EmbedField where
    parseJSON = withObject "EmbedField" $ \obj ->
        EmbedField <$> obj .:  "name"
                   <*> obj .:  "value"
                   <*> obj .:? "inline"

instance ToJSON EmbedField where
    toJSON p = object [ "name"   .= embedFieldName p
                      , "value"  .= embedFieldValue p
                      , "inline" .= embedFieldInline p
                      ]

data Reaction = Reaction deriving Show -- TODO

instance FromJSON Reaction where
    parseJSON = const (pure Reaction)

data Activity = Activity deriving Show -- TODO

instance FromJSON Activity where
    parseJSON = const (pure Activity)

data Application = Application deriving Show -- TODO

instance FromJSON Application where
    parseJSON = const (pure Application)

data VoiceState = VoiceState deriving Show -- TODO

instance FromJSON VoiceState where
    parseJSON = const (pure VoiceState)

data Message = Message
    { messageId              :: Snowflake
    , messageChannelId       :: Snowflake
    , messageGuildId         :: Maybe Snowflake
    , messageAuthor          :: Either User WebhookUser
--    , member :: PARTIAL GuildMember TODO ugh
    , messageContent         :: Text
    , messageTimestamp       :: Text
    , messageEditedTimestamp :: Maybe Text
    , messageTts             :: Bool
    , messageMentionEveryone :: Bool
    , messageMentions        :: [User] -- TODO: these also have a partial "member" GuildMember field :rolling_eyes:
    , messageMentionRoles    :: [Snowflake]
    , messageAttachments     :: [Attachment]
    , messageEmbeds          :: [Embed]
    , messageReactions       :: Maybe [Reaction]
    , messageNonce           :: Maybe Snowflake
    , messagePinned          :: Bool
    , messageWebhookId       :: Maybe Snowflake
    , messageType            :: Int -- TODO: enum
    , messageActivity        :: Maybe Activity
    , messageApplication     :: Maybe Application
    } deriving Show

instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj -> do

        Message <$> obj .:  "id"
                <*> obj .:  "channel_id"
                <*> obj .:? "guild_id"
                <*> (fmap Left (obj .: "author") <|> fmap Right (obj .: "author")) -- could be a user OR a webhook..
                -- <*> obj .:? "member" -- PARTIAL WTF
                <*> obj .:  "content"
                <*> obj .:  "timestamp"
                <*> obj .:? "edited_timestamp"
                <*> obj .:  "tts"
                <*> obj .:  "mention_everyone"
                <*> obj .:  "mentions"
                <*> obj .:  "mention_roles"
                <*> obj .:  "attachments"
                <*> obj .:  "embeds"
                <*> obj .:? "reactions"
                <*> obj .:? "nonce"
                <*> obj .:  "pinned"
                <*> obj .:? "webhook_id"
                <*> obj .:  "type"
                <*> obj .:? "activity"
                <*> obj .:? "application"
