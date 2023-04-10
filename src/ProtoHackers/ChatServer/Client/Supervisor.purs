module ProtoHackers.ChatServer.Client.Supervisor
  ( startLink
  , startChild
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Process (Process)
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , RestartStrategy(..)
  , crashIfChildNotRunning
  )
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import ProtoHackers.ChatServer.Client as Client
import ProtoHackers.ChatServer.Client.Types as ClientTypes

type SupervisorType = Supervisor.SupervisorType ClientTypes.Arguments ClientTypes.Pid
type Pid = Supervisor.SupervisorPid ClientTypes.Arguments ClientTypes.Pid

name :: RegistryName SupervisorType
name = "ProtoHackers.ChatServer.Client.Supervisor" # Atom.atom # Local

startLink :: Effect (StartLinkResult Pid)
startLink = do
  let
    childType = Worker
    intensity = 5
    period = Seconds 10.0
    restartStrategy = RestartTransient
    start = Client.startLink
    shutdownStrategy = 5000.0 # Milliseconds # ShutdownTimeout
    init = pure { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  Supervisor.startLink (Just name) init

startChild :: ClientTypes.Arguments -> Effect ClientTypes.Pid
startChild arguments = do
  crashIfChildNotRunning <$> Supervisor.startChild (ByName name) arguments
