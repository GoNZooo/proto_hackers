module SimpleServer.Types
  ( StartLinkArguments(..)
  , ServerPid(..)
  , InitValue
  , ReturnValue
  , StopReason(..)
  , noReply
  , reply
  , stop
  , initOk
  , initError
  ) where

import Data.Maybe (Maybe)
import Erl.Process (Process, ProcessM)
import Foreign (Foreign)
import Foreign as Foreign
import Pinto.Types (RegistryName)

type StartLinkArguments arguments message state =
  { init :: arguments -> ProcessM message (InitValue state)
  , handleInfo :: message -> state -> ProcessM message (ReturnValue state)
  , name :: Maybe (RegistryName (Process message))
  }

newtype ServerPid :: forall s. Type -> s -> Type
newtype ServerPid message state = ServerPid (Process message)

data InitValue state
  = SimpleInitOk state
  | SimpleInitError Foreign

data ReturnValue state
  = SimpleNoReply state
  | SimpleReply Foreign state
  | SimpleStop StopReason state

data StopReason
  = StopNormal
  | StopShutdown
  | StopOther Foreign

noReply :: forall state. state -> ReturnValue state
noReply state = SimpleNoReply state

reply :: forall state a. a -> state -> ReturnValue state
reply value state = SimpleReply (Foreign.unsafeToForeign value) state

stop :: forall state. StopReason -> state -> ReturnValue state
stop reason state = SimpleStop reason state

initOk :: forall state. state -> InitValue state
initOk state = SimpleInitOk state

initError :: forall state a. a -> InitValue state
initError value = SimpleInitError (Foreign.unsafeToForeign value)

