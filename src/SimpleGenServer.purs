module SimpleGenServer
  ( startLink
  , cast
  , call
  , module SimpleServer.Types
  , module SimpleServer.Utilities
  ) where

import Prelude

import Effect (Effect)
import Erl.Process (Process, ProcessM)
import Pinto.Types (StartLinkResult)
import SimpleServer.Types
  ( ProcessReference
  , Reply
  , ReturnValue
  , ServerPid
  , StartLinkArguments
  , initError
  , initOk
  , noReply
  , reply
  , stop
  )
import SimpleServer.Utilities (sendSelf)

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
   . ProcessReference message state
  -> ((Process a) -> state -> ProcessM message (Reply a state))
  -> Effect a
