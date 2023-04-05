module ProtoHackers.PriceServer.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  ) where

import Prelude

import Erl.Kernel.Inet (ListenSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Pinto.GenServer (ServerPid, ServerType)

data Message = AcceptClients

type State = { socket :: TcpSocket PassiveSocket ListenSocket }

type Arguments = {}

type ServerType' = ServerType Unit Unit Message State
type Pid = ServerPid Unit Unit Message State
