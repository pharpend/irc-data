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
-- Module      : Data.IRC.Nick
-- Description : Parsing and what-not for nicks.
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module Data.IRC.Nick
  (Nick
  ,unNick
  ,parseNick
  ,nickParser)
  where
  
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Monoid

-- |A newtype over 'ByteString'
-- 
-- A nickname is between 1 and 9 characters long. Quoth RFC 2812:
-- 
-- > nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
-- > letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
-- > special    =  %x5B-60 / %x7B-7D
-- >                  ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
newtype Nick = Nick { unNick :: ByteString }
  deriving Eq

parseNick :: ByteString -> Either String Nick
parseNick = parseOnly nickParser          

nickParser :: Parser Nick
nickParser = fmap Nick nnp <?> "nick"
  where nnp =
          do initChr <- satisfy letterSpecialChars
             rest <- fmap B.pack $ many' $ satisfy letterSpecialCharsDigits
             return $ B.cons initChr $ B.take 8 rest
        letterSpecialChars x = B.elem x $ C.pack letterSpecialChars'
        letterSpecialChars' = mconcat [['A' .. 'Z'],['a' .. 'z'],"[]\\`_^{|}"]
        letterSpecialCharsDigits x =
          B.elem x $ C.pack letterSpecialCharsDigits'
        letterSpecialCharsDigits' =
          mappend letterSpecialChars' $ mconcat [['0' .. '9'],"-"]
