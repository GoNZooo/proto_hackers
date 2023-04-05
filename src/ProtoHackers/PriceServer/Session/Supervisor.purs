module ProtoHackers.PriceServer.Session.Supervisor
  ( startLink
  , startChild
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom as Atom
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , RestartStrategy(..)
  , crashIfChildNotRunning
  )
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import ProtoHackers.PriceServer.Session as Session
import ProtoHackers.PriceServer.Session.Types as SessionTypes

type SupervisorType = Supervisor.SupervisorType SessionTypes.Arguments SessionTypes.Pid
type Pid = Supervisor.SupervisorPid SessionTypes.Arguments SessionTypes.Pid

name :: RegistryName SupervisorType
name = "ProtoHackers.PriceServer.Session.Supervisor" # Atom.atom # Local

startLink :: Effect (StartLinkResult Pid)
startLink = do
  let
    childType = Worker
    intensity = 5
    period = Seconds 10.0
    restartStrategy = RestartTransient
    start = Session.startLink
    shutdownStrategy = 5000.0 # Milliseconds # ShutdownTimeout
    init = pure { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  Supervisor.startLink (Just name) init

startChild :: SessionTypes.Arguments -> Effect SessionTypes.Pid
startChild arguments = do
  crashIfChildNotRunning <$> Supervisor.startChild (ByName name) arguments
