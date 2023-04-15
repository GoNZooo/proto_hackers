module ProtoHackers.ChatServer.Presence
  ( startLink
  , getUsers
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom as Atom
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Process (ProcessM)
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.ChatServer.Presence.Bus (UserEvent(..))
import ProtoHackers.ChatServer.Presence.Bus as PresenceBus
import ProtoHackers.ChatServer.Presence.Types (Arguments, Continue, Message, Pid, State, Stop)
import SimpleServer.GenServer (InitValue, ProcessReference(..), ReturnValue, StopReason)
import SimpleServer.GenServer as SimpleServer

serverName :: RegistryName Pid
serverName = "ProtoHackers.ChatServer.Presence" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  SimpleServer.startLink
    arguments
    { name: Just serverName, init, handleInfo, handleContinue, terminate }

getUsers :: Effect (List String)
getUsers = SimpleServer.call (NameReference serverName) \_from state@{ users } -> do
  users
    # Map.values
    # List.sort
    # SimpleServer.reply state
    # pure

init :: Arguments -> ProcessM Message (InitValue State Continue Stop)
init {} = do
  self <- SimpleServer.self
  liftEffect $ PresenceBus.subscribe self identity
  { users: Map.empty } # SimpleServer.initOk # pure

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleInfo (UserJoined { ref, username }) state@{ users } = do
  state { users = Map.insert ref username users } # SimpleServer.noReply # pure
handleInfo (UserLeft { ref }) state@{ users } = do
  state { users = Map.delete ref users } # SimpleServer.noReply # pure
handleInfo (UserSentMessage _r) state = state # SimpleServer.noReply # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleContinue _ state = state # SimpleServer.noReply # pure

terminate :: StopReason Stop -> State -> ProcessM Message Unit
terminate _ _ = pure unit
