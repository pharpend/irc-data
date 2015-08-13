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
-- Module      : Data.IRC
-- Description : Parsing, abstracting, and formatting of IRC messages.
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This is an umbrella module, meaning you do not need to import any submodules
-- of @Data.IRC@.
-- 
-- This module contains tools to parse, abstract, and format raw IRC messages.
-- 
-- Relevant RFCs:
-- 
-- - <https://tools.ietf.org/html/rfc1123 RFC 1123>, regarding valid
-- 'HostName's.
-- 
-- - <https://tools.ietf.org/html/rfc1459 RFC 1459>, the first IRC
-- specification.
-- 
-- - <https://tools.ietf.org/html/rfc2812 RFC 2812>, an update on RFC 1459.
-- 
-- You might also read:
-- 
-- - <https://en.wikipedia.org/wiki/Internet_Relay_Chat>
-- - <https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands>

module Data.IRC 
       (module Data.IRC.Nick
       ,module Data.IRC.Server)
       where

import Data.IRC.Nick
import Data.IRC.Server
