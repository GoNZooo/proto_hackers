module SimpleServer.Types
  ( StartLinkArguments(..)
  , ServerPid(..)
  , ProcessReference(..)
  , InitValue
  , ReturnValue
  , Reply
  , StopReason(..)
  , noReply
  , reply
  , replyAndStop
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
  , name :: Maybe (RegistryName (ServerPid message state))
  }

newtype ServerPid :: forall s. Type -> s -> Type
newtype ServerPid message state = ServerPid (Process message)

data ProcessReference :: forall s. Type -> s -> Type
data ProcessReference message state
  = PidReference (ServerPid message state)
  | NameReference (RegistryName (ServerPid message state))

data InitValue state
  = SimpleInitOk state
  | SimpleInitError Foreign

data ReturnValue state
  = SimpleNoReply state
  | SimpleStop StopReason state

data StopReason
  = StopNormal
  | StopShutdown
  | StopOther Foreign

data Reply a state
  = SimpleCallReply a state
  | SimpleCallStop a StopReason state

noReply :: forall state. state -> ReturnValue state
noReply state = SimpleNoReply state

reply :: forall state a. state -> a -> Reply a state
reply state reply' = SimpleCallReply reply' state

stop :: forall state. StopReason -> state -> ReturnValue state
stop reason state = SimpleStop reason state

replyAndStop :: forall state a. state -> StopReason -> a -> Reply a state
replyAndStop state reason reply' = SimpleCallStop reply' reason state

initOk :: forall state. state -> InitValue state
initOk state = SimpleInitOk state

initError :: forall state a. a -> InitValue state
initError value = SimpleInitError (Foreign.unsafeToForeign value)

