module ProtoHackers.ChatServer.Client.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  ) where

import Prelude

import Data.Maybe (Maybe)
import Erl.Kernel.Inet (ConnectedSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Process (Process)
import Erl.Types (Ref)
import Pinto.GenServer (ServerType)
import ProtoHackers.ChatServer.Presence.Bus (UserEvent)

data Message
  = ReadUsername
  | ReadChatMessage
  | PresenceEvent UserEvent

type State =
  { socket :: TcpSocket PassiveSocket ConnectedSocket
  , username :: Maybe String
  , ref :: Ref
  }

type Arguments = { socket :: TcpSocket PassiveSocket ConnectedSocket }

type ServerType' = ServerType Unit Unit Message State
type Pid = Process Message
