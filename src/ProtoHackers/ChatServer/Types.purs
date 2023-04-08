module ProtoHackers.ChatServer.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  ) where

import Prelude

import Erl.Kernel.Inet (ListenSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Process (Process)
import Pinto.GenServer (ServerType)

data Message = Accept

type State = { socket :: TcpSocket PassiveSocket ListenSocket }

type Arguments = {}

type ServerType' = ServerType Unit Unit Message State
type Pid = Process Message
