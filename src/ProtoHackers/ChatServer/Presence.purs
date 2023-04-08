module ProtoHackers.ChatServer.Presence
  ( startLink
  , getUsers
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Process (Process, ProcessM)
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.ChatServer.Presence.Bus (UserEvent(..))
import ProtoHackers.ChatServer.Presence.Bus as PresenceBus
import ProtoHackers.ChatServer.Presence.Types (Arguments, Message, State)
import SimpleGenServer (ServerPid)
import SimpleGenServer as SimpleServer
import SimpleServer.Types (InitValue, ProcessReference(..), ReturnValue)

serverName :: RegistryName (ServerPid Message State)
serverName = "ProtoHackers.ChatServer.Presence" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { name: Just serverName, init, handleInfo }

getUsers :: Effect (List String)
getUsers = SimpleServer.call (NameReference serverName) \_from state@{ users } -> do
  users
    # Map.values
    # List.sort
    # SimpleServer.reply state
    # pure

init :: Arguments -> ProcessM Message (InitValue State)
init {} = do
  _subscriptionRef <- PresenceBus.subscribe identity
  { users: Map.empty } # SimpleServer.initOk # pure

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State)
handleInfo (UserJoined { ref, username }) state@{ users } = do
  state { users = Map.insert ref username users } # SimpleServer.noReply # pure
handleInfo (UserLeft { ref }) state@{ users } = do
  state { users = Map.delete ref users } # SimpleServer.noReply # pure
handleInfo (UserSentMessage _r) state = state # SimpleServer.noReply # pure

