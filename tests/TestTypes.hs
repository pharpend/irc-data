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
-- Module      : TestTypes
-- Description : Arbitrary instances and types for the test suite
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module TestTypes where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck

instance Arbitrary ByteString where
  arbitrary = fmap C.pack arbitrary
  
instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary         

newtype WSpace = WSpace ByteString
  deriving (Eq, Show)        

instance Arbitrary WSpace where
  arbitrary = 
    fmap (WSpace . C.pack) $ listOf (suchThat arbitrary isSpace)
