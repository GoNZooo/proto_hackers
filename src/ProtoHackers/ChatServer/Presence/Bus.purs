module ProtoHackers.ChatServer.Presence.Bus
  ( bus
  , subscribe
  , unsubscribe
  , publish
  , UserEvent(..)
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Erl.Process (class HasSelf)
import Erl.Types (Ref)
import SimpleBus (Bus, SubscriptionRef)
import SimpleBus as SimpleBus

data UserEvent
  = UserJoined { ref :: Ref, username :: String }
  | UserLeft { ref :: Ref, username :: String }
  | UserSentMessage { ref :: Ref, username :: String, message :: String }

bus :: Bus Unit UserEvent
bus = SimpleBus.bus unit

subscribe
  :: forall message m
   . HasSelf m message
  => MonadEffect m
  => (UserEvent -> message)
  -> m SubscriptionRef
subscribe f = SimpleBus.subscribe bus f

unsubscribe :: SubscriptionRef -> Effect Unit
unsubscribe = SimpleBus.unsubscribe

publish :: UserEvent -> Effect Unit
publish message = SimpleBus.raise bus message

