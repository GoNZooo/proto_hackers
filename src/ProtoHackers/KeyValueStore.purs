module ProtoHackers.KeyValueStore
  ( startLink
  , increment
  , currentCount
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Process (Process, ProcessM)
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.KeyValueStore.Types (Arguments, Message(..), State, Pid)
import SimpleServer.GenServer (InitValue, ProcessReference(..), ReturnValue)
import SimpleServer.GenServer as SimpleServer

serverName :: RegistryName Pid
serverName = "ProtoHackers.KeyValueStore" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { name: Just serverName, init, handleInfo }

init :: Arguments -> ProcessM Message (InitValue State)
init { initialCount } = { count: initialCount } # SimpleServer.initOk # pure

increment :: Effect Unit
increment = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.noReply $ state { count = state.count + 1 }

currentCount :: Effect Int
currentCount = SimpleServer.call (NameReference serverName) \_from state ->
  pure $ SimpleServer.reply state state.count

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State)
handleInfo NoOp state = state # SimpleServer.noReply # pure
