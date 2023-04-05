module Test.SessionRequestSpec
  ( main
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import ProtoHackers.PriceServer.Session as Session
import ProtoHackers.PriceServer.Session.Types (Request(..))
import PurerlTest (assertEqual, runSuites, suite, test)

main :: Effect Unit
main = do
  runSuites do
    suite "SessionRequest" do
      test "parsing insert" do
        Session.parseRequest "\x49\x00\x00\x30\x39\x00\x00\x00\x65" `assertEqual`
          Right (Insert { timestamp: 12345, price: 101 })
        Session.parseRequest "\x49\x00\x00\x30\x3a\x00\x00\x00\x66" `assertEqual`
          Right (Insert { timestamp: 12346, price: 102 })
        Session.parseRequest "\x49\x00\x00\x30\x3b\x00\x00\x00\x64" `assertEqual`
          Right (Insert { timestamp: 12347, price: 100 })
        Session.parseRequest testPrice1 `assertEqual`
          Right (Insert { timestamp: 40960, price: 5 })

foreign import testPrice1 :: String
