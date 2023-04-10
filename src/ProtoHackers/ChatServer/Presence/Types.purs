module ProtoHackers.ChatServer.Presence.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  , Continue
  ) where

import Prelude

import Erl.Data.Map (Map)
import Erl.Types (Ref)
import Pinto.GenServer (ServerType)
import ProtoHackers.ChatServer.Presence.Bus as PresenceBus
import SimpleServer.GenServer (ServerPid)

type Continue = Unit

type Message = PresenceBus.UserEvent

type State = { users :: Map Ref String }

type Arguments = {}

type ServerType' = ServerType Unit Unit Message State
type Pid = ServerPid Message State Continue
