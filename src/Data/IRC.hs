{-# LANGUAGE CPP #-}

-- Copyright 2015 Peter Harpending
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--  http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | 
-- Module      : Data.IRC
-- Description : Parsing, abstracting, and formatting of IRC messages.
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This is an umbrella module, meaning you do not need to import any
-- submodules of @Data.IRC@.
-- 
-- This module contains tools to parse, abstract, and format raw IRC
-- messages.
-- 
-- Relevant RFCs:
-- 
-- - <https://tools.ietf.org/html/rfc1123 RFC 1123>, regarding valid
--   'HostName's.
-- - <https://tools.ietf.org/html/rfc1459 RFC 1459>, the first IRC
--   specification.
-- - <https://tools.ietf.org/html/rfc2812 RFC 2812>, an update on RFC
--   1459.
-- 
-- You might also read:
-- 
-- - <https://en.wikipedia.org/wiki/Internet_Relay_Chat>
-- - <https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands>

module Data.IRC 
  (UserCommand(..)
   -- ** Newtypes and their construction
  ,HostName
  ,unHostName
  ,mkHostName
  ,mkHostNameEither
  ,hostNameParser
  ,ChannelName
  ,unChannelName
  ,mkChannelName
  ,mkChannelNameEither
  ,channelNameParser
  ,Nick
  ,unNick
  ,mkNick
  ,mkNickEither
  ,nickParser
   -- *** These will eventually be newtypes, for now they're normal ol' 
  --- *** @type@ aliases..
  ,Key
  ,ServerMask)
  where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.HashMap.Strict (HashMap)
import Data.Ord (comparing)
import Data.Vector (Vector)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-- |The type for user commands.
-- 
-- See
-- <https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands#User_commands>
-- for more details.
data UserCommand
  =
    -- |List the administrators of an optionally specified server. If no
    -- server is specified, use the current server.
    Admin (Maybe HostName)
  |
    -- |Set client status as away, with a message. If no message is
    -- specified, remove away status.
    Away (Maybe ByteString)
  |
    -- |Not defined in the RFC, but some IRC networks use it. It notifies a
    -- person (the 'Nick') in the given channel, bypassing any flood
    -- protection. This can only be used by channel operators.
    CNotice Nick
            ChannelName
            ByteString
  |
    -- |Sends a private message to the 'Nick' in the 'ChannelName' that
    -- bypasses any flood protection. This can only be used by channel
    -- operators.
    --
    -- This is not defined in the RFC, but some IRC networks use it.
    CPrivMsg Nick
             ChannelName
             ByteString
  |
    -- | Instructs the server to connect to the target server on the
    -- target port.
    -- 
    -- If a host server is specified, forward the command to the other
    -- server.
    --
    -- May only be issued by IRC network operators.
    Connect HostName
            Int
            (Maybe HostName)
  |
    -- |Instructs server to kill itself. May only be used by IRC network
    -- operators.
    Die
  |
    -- |For internal use by servers. Transport a command from the source
    -- hostname to the destintation hostname.
    Encap HostName
          HostName
          IRCCommand
  |
    -- |Used by servers to communicate errors to other servers. It is
    -- also used before terminating a client.
    Error ByteString
  |
    -- |Send a help message to the server. Not specified in any RFC, but
    -- used by most IRC daemons.
    Help
  |
    -- |Request information about the target server, or the current
    -- server if the target is omitted.
    Info (Maybe HostName)
  |
    -- |Invite a person to a channel.
    --
    -- 1. If the channel exists, only people already in the channel can
    --    send this command.
    -- 2. If the channel exists, and has mode +i, then only channel
    --    operators can send it.
    -- 3. If the channel does not exist, anyone can send this.
    Invite Nick
           ChannelName
  |
    -- |Queries the server to see if people in the (space separated)
    -- list are currently on the network.
    ISON (Vector Nick)
  |
    -- |Join the specified channels, comma-separated. Optionally specify
    -- keys to join each channel.
    Join (Vector ChannelName)
         (HashMap ChannelName Key)
  |
    -- |Kick a person from a channel. This may only be used by channel
    -- operators.
    Kick ChannelName
         Nick
         (Maybe ByteString)
  |
    -- |Kill a client's connection to the server, with a message.
    Kill Nick
         ByteString
  |
    -- |Sends a notice to an invite-only channel with a message,
    -- requesting an invite. This is supported by most IRC daemons, but
    -- not in any RFC.
    Knock ChannelName (Maybe ByteString)
  |
    -- |Lists all server links matching the server mask.
    Links (Maybe (HostName, Maybe ServerMask))
  |
    -- |List all channels on a server. If comma-separated channels are
    -- given, list their respective 'Topic's. If a 'HostName' is given,
    -- forward the request to that server.
    List (Maybe (Vector ChannelName, Maybe HostName))
  |
    -- |Statistics about the size of the network. If called with no
    -- arguments, reflect entire network. Given a server mask, restrict
    -- the statistics to that server mask. If a hostname is given,
    -- forward the message to the target.
    Lusers (Maybe (ServerMask, Maybe HostName))
  |
    -- |Send a message to either a channel or a nick with a message.
    PrivMsg (Either ChannelName Nick)
            ByteString

-- |A newtype over 'ByteString'.
-- 
-- This is something of the form @foo.bar.baz.quux@. 
-- 
-- The acceptable chars are ASCII alphabet, digits, and the characters
-- @.-@.
-- 
-- The first character must be alphanumeric.
-- 
-- See <https://tools.ietf.org/html/rfc1123 RFC 1123> for more details.
newtype HostName = HostName {unHostName :: ByteString}
  deriving (Eq)

instance Show HostName where
  show = show . unHostName

instance Ord HostName where
   compare = comparing unHostName

-- |Try to parse a 'ByteString' into a 'HostName'
mkHostName :: ByteString -> Maybe HostName
mkHostName = maybeResult . parse hostNameParser 

-- |Try to parse a 'ByteString' into a 'HostName'
mkHostNameEither :: ByteString -> Either String HostName
mkHostNameEither = eitherResult . parse hostNameParser 

-- |Parse a host name
hostNameParser :: Parser HostName
hostNameParser =
  fmap HostName $ (hnp <?> "hostname")
  where hnp =
          do firstChar <- satisfy alphaNum <?> "first character in hostname"
             rest <-
               takeWhile1
                 (\x ->
                    alphaNum x || inClass "._-" x) <?>
               "rest of characters in hostname"
             return (mappend (B8.singleton firstChar) rest)
        alphaNum x = isAlpha_ascii x || isDigit x

-- |A newtype over 'ByteString'.
-- 
-- Quoth RFC 2812, section 1.3:
-- 
-- Channels names are strings (beginning with a '&', '#', '+' or '!'
-- character) of length up to fifty (50) characters.  Apart from the
-- requirement that the first character is either '&', '#', '+' or '!',
-- the only restriction on a channel name is that it SHALL NOT contain
-- any spaces (' '), a control G (^G or ASCII 7), a comma (',').
-- 
-- Unquoth
-- 
-- Colons (:) are also banned, as they are used to delimit channel
-- masks.
newtype ChannelName = ChannelName {unChannelName :: ByteString}
  deriving (Eq)

instance Ord ChannelName where
   compare = comparing unChannelName

instance Show ChannelName where
  show = show . unChannelName

-- |Try to parse a 'ByteString' into a 'ChannelName'
mkChannelName :: ByteString -> Maybe ChannelName
mkChannelName = maybeResult . parse channelNameParser 

-- |Try to parse a 'ByteString' into a 'ChannelName'
mkChannelNameEither :: ByteString -> Either String ChannelName
mkChannelNameEither = eitherResult . parse channelNameParser 

-- |Parse a channel name
channelNameParser :: Parser ChannelName
channelNameParser =
  fmap ChannelName $
  do firstChar <- satisfy (inClass "#&!+") <?> 
                  "Initial character in channel name"
     rest <- takeWhile1 (notInClass " \0x7,:") <?>
             "Non-initial characters in channel name"
     if B8.length rest < 50
        then return (mappend (B8.singleton firstChar) rest)
        else fail "Channel name must not be longer than 50 characters."

-- |A newtype over 'ByteString'.
-- 
-- Quoth RFC 2812, section 1.2.1:
-- 
-- Each user is distinguished from other users by a unique nickname
-- having a maximum length of nine (9) characters.
-- 
-- Unquoth
newtype Nick = Nick {unNick :: ByteString}
  deriving (Eq)

instance Ord Nick where
   compare = comparing unNick

instance Show Nick where
  show = show . unNick

-- |Try to parse a 'ByteString' into a 'Nick'
mkNick :: ByteString -> Maybe Nick
mkNick = maybeResult . parse nickParser 

-- |Try to parse a 'ByteString' into a 'Nick'
mkNickEither :: ByteString -> Either String Nick
mkNickEither = eitherResult . parse nickParser 

-- |Parse a nick name
-- 
-- A nick may start with a letter, or one of
-- 
-- > [\]^_`{|}
-- 
-- The remaining characters can be
-- 
-- * one of the allowed initial characters,
-- * a digit, or
-- * a hyphen (-).
nickParser :: Parser Nick
nickParser =
  fmap Nick $
  do firstChar <- satisfy (\x -> isAlpha_ascii x || 
                                 inClass nickInit x) <?> 
                  "Initial character in nick"
     rest <- takeWhile1 (\x -> or [isAlpha_ascii x
                                  ,isDigit x
                                  ,inClass ('-':nickInit) x]) <?>
             "Non-initial characters in channel name"
     if B8.length rest < 9
        then return (mappend (B8.singleton firstChar) rest)
        else fail "Nick may not be longer than 9 characters."
  where nickInit = "[\\]^_`{|}"

-- |Will eventually be a newtype over 'ByteString'
type Key = ByteString
-- |Will eventually be a newtype over 'ByteString'
type ServerMask = ByteString
