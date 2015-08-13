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

module Data.IRC.Channel where

