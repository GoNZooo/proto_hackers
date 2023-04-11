module ProtoHackers.ChatServer.Presence.Bus
  ( subscribe
  , unsubscribe
  , publish
  , UserEvent(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Atom as Atom
import Erl.Process (Process)
import Erl.Types (Ref)
import PgBus (Bus)
import PgBus as PgBus

data UserEvent
  = UserJoined { ref :: Ref, username :: String }
  | UserLeft { ref :: Ref, username :: String }
  | UserSentMessage { ref :: Ref, username :: String, message :: String }

derive instance Generic UserEvent _

instance Show UserEvent where
  show = genericShow

bus :: Bus Atom UserEvent
bus = PgBus.bus (Atom.atom "ProtoHackers.ChatServer.Presence.Bus")

subscribe
  :: forall message
   . Process message
  -> (UserEvent -> message)
  -> Effect Unit
subscribe pid f = do
  PgBus.subscribe bus f pid

unsubscribe :: forall message. Process message -> Effect Unit
unsubscribe pid = PgBus.unsubscribe bus pid

publish :: UserEvent -> Effect Unit
publish message = do
  PgBus.publish bus message

