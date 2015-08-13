{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- Copyright 2015 Peter Harpending
-- 
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License.  You may obtain a copy
-- of the License at
-- 
--  http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
-- License for the specific language governing permissions and limitations under
-- the License.

-- | 
-- Module      : Data.IRC.Hostname
-- Description : The parsers and stuff for hostnames
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Data.IRC.Hostname
       (Server
       ,unServer
       ,mkServer
       ,HostName
       ,unHostName
       ,parseHostName
       ,hostNameParser
       )
       where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Ord (comparing)
import Prelude hiding (takeWhile)

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Monoid
#endif

-- |A newtype over 'HostName'. 
-- 
-- The restriction is, a 'ServerName' must be at most 63 characters long.
-- 
-- Quoth RFC 2812, Section 1.1:
-- 
-- Servers are uniquely identified by their name, which has a maximum length of
-- sixty three (63) characters.
-- 
-- Server names, as defined by RFC 2812, section 2.3.1:
-- 
-- > servername =  hostname
newtype Server = Server {unServer :: HostName}
  deriving Eq

instance Ord Server where
  compare = comparing unServer

instance Show Server where
  show = show . unServer
  
-- |This discards any input after 63 characters.
mkServer :: HostName -> Server
mkServer (HostName x) = Server (HostName (B.take 63 x))

-- |A newtype over 'ByteString'
-- 
-- As defined by RFC 2812, section 2.3.1:
-- 
-- > hostname   =  shortname *( "." shortname )
-- > shortname  =  ( letter / digit ) *( letter / digit / "-" )
-- >               *( letter / digit )
-- >                 ; as specified in RFC 1123
newtype HostName = HostName {unHostName :: ByteString}
  deriving Eq

instance Ord HostName where
  compare = comparing unHostName

instance Show HostName where
  show = B.unpack . unHostName

-- |Parse a 'HostName', returning 'Left' on invalid input, with an error
-- message.
parseHostName :: ByteString -> Either String HostName
parseHostName = parseOnly hostNameParser

-- |A 'Parser' for 'HostName's
hostNameParser :: Parser HostName
hostNameParser = fmap HostName hnp <?> "hostname"
  where 
    hnp = 
      do initChr <- satisfy alphaNum <?> "first character in shortname"
         rest <- takeWhile alphaNumOrDash <?> "rest of chars in shortname"
         let singleInit = B.singleton initChr
         -- If there aren't any more characters, make sure the last character is
         -- alphanumeric.
         soFar <- 
           if | B.null rest -> pure singleInit
              | alphaNum (B.last rest) -> pure (mappend singleInit rest)
              | otherwise -> 
                  fail "Last character in shortname must be alphanumeric"
         -- Look for a dot.
         maybeDot <- peekChar
         case maybeDot of
           -- If there is a dot, return what we have so far, plus whatever's
           -- next
           Just '.' -> do char '.'
                          after <- hnp
                          return (mconcat [soFar,".",after])
           -- If we're at the end of input, or there's something that isn't
           -- a dot, just return what we have so far.
           _ -> pure soFar
    alphaNum x = isAlpha_ascii x || isDigit x
    alphaNumOrDash x = alphaNum x || x == '-'
