module SimpleServer.Utilities
  ( sendSelf
  ) where

import Data.Unit (Unit)
import Erl.Process (ProcessM)

foreign import sendSelf :: forall message. message -> ProcessM message Unit
