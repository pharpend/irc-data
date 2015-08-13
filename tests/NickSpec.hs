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
-- Module      : NickSpec
-- Description : Tests for nick parsing and what not
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module NickSpec where

import Data.IRC
import Instances ()

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = context "Nick parsing" $ do
  specify "empty nick is invalid" $ 
    shouldSatisfy (parseNick "") isLeft
  specify "nicks longer than 9 chars are invalid" $ 
    property $ \(LongNick longNick) ->
        shouldSatisfy (parseNick longNick) isLeft

newtype ValidNick = ValidNick ByteString
  deriving (Eq, Show)

instance Arbitrary ValidNick where
  arbitrary = do LongNick x <- arbitrary
                 return $ ValidNick $ B.take 9 x

newtype LongNick = LongNick ByteString
  deriving (Eq, Show)

instance Arbitrary LongNick where
  arbitrary = suchThat arbitrary' (\(LongNick x) -> B.length x > 9)
    where 
      arbitrary' = do firstChar <- elements validInitialNickChars
                      rest <- listOf $ elements validNonInitialNickChars
                      return $ LongNick $ C.pack $ firstChar : rest


