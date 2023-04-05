module ProtoHackers.PriceServer.Session.Types
  ( Message(..)
  , ServerType'
  , Pid
  , State
  , Arguments
  , Request(..)
  , InvalidRequest(..)
  , PriceData
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Erl.Data.Set (Set)
import Erl.Kernel.Inet (ConnectedSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Pinto.GenServer (ServerPid, ServerType)

data Message = ReadRequest

type State =
  { socket :: TcpSocket PassiveSocket ConnectedSocket
  , prices :: Set PriceData
  , requestCount :: Int
  }

type Arguments = { socket :: TcpSocket PassiveSocket ConnectedSocket }

type ServerType' = ServerType Unit Unit Message State
type Pid = ServerPid Unit Unit Message State

data Request
  = Insert PriceData
  | Query { minimumTimestamp :: Int, maximumTimestamp :: Int }

derive instance Eq Request
derive instance Generic Request _

instance Show Request where
  show = genericShow

type PriceData = { timestamp :: Int, price :: Int }

newtype InvalidRequest = InvalidRequest String

derive instance Eq InvalidRequest
derive instance Newtype InvalidRequest _

