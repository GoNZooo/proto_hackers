module ProtoHackers.ChatServer.Presence.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  ) where

import Prelude

import Erl.Data.Map (Map)
import Erl.Types (Ref)
import Pinto.GenServer (ServerPid, ServerType)
import ProtoHackers.ChatServer.Presence.Bus as PresenceBus

type Message = PresenceBus.UserEvent

type State = { users :: Map Ref String }

type Arguments = {}

type ServerType' = ServerType Unit Unit Message State
type Pid = ServerPid Unit Unit Message State
