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
-- Module      : ServerSpec
-- Description : Runs the test suite for server parsing
-- Copyright   : Copyright 2015 Peter Harpending
-- License     : Apache-2.0
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : portable

module ServerSpec where

import           Data.IRC
import           TestTypes ()

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.QuickCheck


spec :: Spec
spec = parallel $ do
  context "Hostnames" $ do
    context "Supposedly valid hostnames" $
      mapM_ testValidHnom
        [ "irc.freenode.net"
        , "chat.freenode.net"
        , "foo.bar.baz"
        , "irc.oftc.net"
        , "blah.bleh"
        , "foo.bar.baz"
        , "foobar.baz.qu123ux"
        , "foo-bar.baz.qu123ux"
        , "foobar.baz.qu-123ux"
        , "foo-bar.baz.qu-123ux"
        , "fero.tui.sceleris"
        ]
    context "Supposedly invalid hostnames" $ do
      mapM_ testInvalidHnom
        [ "irc.freenode.net."
        , "chat-.freenode.net"
        , "foo,.--8bar.baz"
        , "irc.oftc-.net"
        , "blah.blehj61.--"
        , "foo.bar.baz--8-"
        , "foobar.baz..qu123ux"
        , "foo-.-bar.baz.qu123ux"
        , "foob.-.ar.baz.qu-123ux"
        , "foo-.bar.baz.qu-123ux"
        , "fer.-o.tui.sceleris"
        ]
      specify "Randomly generated invalid hostnames should be invalid" $
        property $
          \(InvalidHostName x) -> 
            shouldNotBe (fmap unHostName (parseHostName (T.encodeUtf8 x))) 
                        (Right x)

testValidHnom :: ByteString -> Spec
testValidHnom s =
  specify (mappend (C.unpack s) " should be a valid hostname") $ do
    shouldBe (fmap unHostName (parseHostName s)) (Right (T.decodeUtf8 s))

testInvalidHnom :: ByteString -> Spec
testInvalidHnom s =
  specify (mappend (C.unpack s) " should not be a valid hostname") $
    shouldNotBe (fmap unHostName (parseHostName s)) (Right (T.decodeUtf8 s))

newtype InvalidHostName = InvalidHostName Text
  deriving (Show, Eq)

newtype HostNamePart = HostNamePart { unHp :: Text }
  deriving (Show, Eq)

instance Arbitrary InvalidHostName where
  arbitrary = 
    do hnparts <- suchThat arbitrary (\x -> length x > 0)
       pure (InvalidHostName (T.intercalate "." (fmap unHp hnparts)))

instance Arbitrary HostNamePart where
  arbitrary =
    fmap HostNamePart $
    suchThat arbitrary $
    \bytes ->
      or [T.null bytes
         ,T.head bytes == '-'
         ,T.last bytes == '-'
         ,T.any (not .
                 (`elem` (mconcat [['A' .. 'Z'],['a' .. 'z'],['0' .. '9'],"-"])))
                bytes]
