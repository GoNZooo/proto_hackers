module ProtoHackers.KeyValueStore.Types
  ( Message(..)
  , Pid
  , State
  , Arguments
  , Command(..)
  ) where

import Erl.Data.Map (Map)
import Erl.Kernel.Inet (PassiveSocket)
import Erl.Kernel.Udp (UdpSocket)
import SimpleServer.GenServer (ServerPid)

data Command
  = Query { key :: String }
  | Insert { key :: String, value :: String }

data Message = Read

type State = { socket :: UdpSocket PassiveSocket, store :: Map String String }

type Arguments = {}

type Pid = ServerPid Message State
