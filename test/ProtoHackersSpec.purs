module Test.ProtoHackersSpec
  ( main
  ) where

import Prelude

import Effect (Effect)
import PurerlTest (assert, assertEqual, assertNotEqual, runSuites, suite, test)

main :: Effect Unit
main = do
  runSuites do
    suite "ProtoHackers" do
      test "assertion" do
        assert true
      test "equality" do
        1 `assertEqual` 1
      test "inequality" do
        1 `assertNotEqual` 2
