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
-- Module      : Data.IRC.Channel
-- Description : Parsing and what not for channel names
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- From RFC 2812, Section 1.3:
-- 
-- > Channels names are strings (beginning with a '&', '#', '+' or '!'
-- > character) of length up to fifty (50) characters.  Apart from the
-- > requirement that the first character is either '&', '#', '+' or '!',
-- > the only restriction on a channel name is that it SHALL NOT contain
-- > any spaces (' '), a control G (^G or ASCII 7), a comma (',').  Space
-- > is used as parameter separator and command is used as a list item
-- > separator by the protocol).  A colon (':') can also be used as a
-- > delimiter for the channel mask.  Channel names are case insensitive.

module Data.IRC.Channel 
       ( Channel
       , unChannel
       , parseChannel
       , channelParser
       , chanString
       , chanStringValidChars
       ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.CaseInsensitive
import           Data.Char (chr)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Prelude hiding (take)

-- |Quoth RFC 2812:
-- 
-- > channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
-- >               [ ":" chanstring ]
-- > chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
-- > chanstring =/ %x2D-39 / %x3B-FF
-- >                  ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
-- > channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
-- 
-- With regard to 
-- 
-- > chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
-- 
-- I think this is a typo, as ASCII 7 is excplicitly not allowed (see module
-- description).
-- 
-- So, for the time being, this module doesn't allow ASCII 7.
newtype Channel = Channel { unChannel :: CI Text }

parseChannel :: ByteString -> Either String Channel
parseChannel = parseOnly channelParser             

channelParser :: Parser Channel
channelParser = 
  fmap (Channel . mk . T.decodeUtf8) cp 
  <?> "channel name"
  where 
    cp = do initialBlock <- 
              alt [ C.singleton <$> A.char '&'
                  , C.singleton <$> A.char '#'
                  , C.singleton <$> A.char '+'
                  , do fst' <- A.char '!'
                       rest <- take 5
                       if C.any (not . 
                                 (`elem` (mappend ['A'..'Z'] 
                                                  ['0'..'9']))) 
                                rest
                          then fail (mappend "Invalid channel id: " 
                                             (show rest))
                          else return $ C.cons fst' rest
                  ]
            cs <- chanString
            rest <- 
              option mempty
                     (do fst' <- A.char ':'
                         rest <- chanString
                         return $ C.cons fst' rest)
            let finalChanName = mconcat [ initialBlock 
                                        , cs
                                        , rest
                                        ]
            if C.length finalChanName > 50
              then fail "Channel name must be less than 50 chars long"
              else return finalChanName
    alt [] = empty
    alt (x : xs) = x <|> alt xs

-- |Note that this does not check length
chanString :: Parser ByteString
chanString = 
  A.takeWhile1 (`elem` chanStringValidChars) <?> "chanstring"


chanStringValidChars :: [Char]
chanStringValidChars = 
  fmap chr $
  mconcat [ [0x01 .. 0x06] 
          , [0x08 .. 0x09]
          , [0x0B .. 0x0C]
          , [0x0E .. 0x1F]
          , [0x21 .. 0x2B]
          , [0x2D .. 0x39]
          , [0x3B .. 0xFF]
          ]


