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
-- Module      : Data.IRC.Service
-- Description : The type for IRC "Services"
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- IRC Services, as specified in section 1.2.2 of RFC 2812:
-- 
-- > Each service is distinguished from other services by a service name composed
-- > of a nickname and a server name.  As for users, the nickname has a maximum
-- > length of nine (9) characters.  See the protocol grammar rules (section
-- > 2.3.1) for what may and may not be used in a nickname.

module Data.IRC.Service where

import Data.IRC.Nick
import Data.IRC.Server

data Service = Service { serviceNick :: Nick
                       , serviceServer :: Server
                       }
  deriving (Eq, Show)
