module ProtoHackers.KeyValueStore.Types
  ( Message(..)
  , Pid
  , State
  , Arguments
  ) where

import SimpleServer.GenServer (ServerPid)

data Message = NoOp

type State = { count :: Int }

type Arguments = { initialCount :: Int }

type Pid = ServerPid Message State
