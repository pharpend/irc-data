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
-- Description : Parsing, abstracting, and formatting of RFC 2812 IRC
--               messages.
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable
-- 
-- This is an umbrella module, meaning you do not need to import any
-- submodules of @Data.IRC@.
-- 
-- This module contains tools to parse, abstract, and format IRC
-- messages compliant with <https://tools.ietf.org/html/rfc2812 RFC
-- 2812> and .

module Data.IRC where

-- |The type for user 
