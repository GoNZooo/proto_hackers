module ProtoHackers.ChatServer.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  , Continue
  ) where

import Prelude

import Erl.Kernel.Inet (ListenSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Pinto.GenServer (ServerType)
import SimpleServer.GenServer (ServerPid)

type Continue = Unit

data Message = Accept

type State = { socket :: TcpSocket PassiveSocket ListenSocket }

type Arguments = {}

type ServerType' = ServerType Unit Unit Message State
type Pid = ServerPid Message State Continue
