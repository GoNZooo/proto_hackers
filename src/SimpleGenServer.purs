module SimpleGenServer
  ( startLink
  , InitValue
  , ReturnValue
  , StopReason(..)
  , cast
  , call
  , ServerPid(..)
  , noReply
  , reply
  , stop
  , initOk
  , initError
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Process (Process, ProcessM)
import Foreign (Foreign)
import Foreign as Foreign
import Pinto.Types (RegistryName, StartLinkResult)

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

startLink
  :: forall arguments message state
   . arguments
  -> StartLinkArguments arguments message state
  -> Effect (StartLinkResult (Process message))
startLink startArguments arguments = do
  startLink_ startArguments arguments

foreign import startLink_
  :: forall arguments message state
   . arguments
  -> StartLinkArguments arguments message state
  -> Effect (StartLinkResult (Process message))

foreign import cast
  :: forall message state
   . ServerPid message state
  -> (state -> ProcessM message Unit)
  -> Effect Unit

foreign import call
  :: forall message state a
   . ServerPid message state
  -> (state -> ProcessM message a)
  -> Effect a
