module ProtoHackers.KeyValueStore.Types
  ( Message(..)
  , Pid
  , State
  , Arguments
  , Command(..)
  , Continue
  , Stop
  ) where

import Prelude

import Erl.Data.Map (Map)
import Erl.Kernel.Inet (PassiveSocket)
import Erl.Kernel.Udp (UdpSocket)
import SimpleServer.GenServer (ServerPid)

type Stop = Void

type Continue = Unit

data Command
  = Query { key :: String }
  | Insert { key :: String, value :: String }

data Message = Read

type State = { socket :: UdpSocket PassiveSocket, store :: Map String String }

type Arguments = {}

type Pid = ServerPid Message State Continue Stop
