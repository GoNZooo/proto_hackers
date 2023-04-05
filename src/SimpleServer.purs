module SimpleServer
  ( startLink
  , InitValue(..)
  , ReturnValue(..)
  ) where

import Effect (Effect)
import Erl.Process (Process, ProcessM)
import Foreign (Foreign)
import Pinto.Types (StartLinkResult)

type StartLinkArguments arguments message state =
  { init :: arguments -> ProcessM message (InitValue state)
  , handleInfo :: message -> state -> ProcessM message (ReturnValue state)
  }

newtype ServerPid :: forall s. Type -> s -> Type
newtype ServerPid message state = ServerPid (Process message)

data InitValue state
  = SimpleInitOk state
  | SimpleInitError Foreign

data ReturnValue state
  = SimpleNoReply state
  | SimpleStop state

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
