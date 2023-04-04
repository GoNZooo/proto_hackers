module ProtoHackers.Supervisor
  ( startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List as ErlList
import Erl.Process.Raw (Pid)
import Pinto.Supervisor (SupervisorPid)
import Pinto.Supervisor as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)
import ProtoHackers.EchoServer as EchoServer
import ProtoHackers.Supervisor.Helpers as SupervisorHelpers

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom supervisorName) $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  supervisorName = "ProtoHackers.Supervisor"
  childSpecs = ErlList.fromFoldable
    [ SupervisorHelpers.worker "ProtoHackers.TcpEchoServer" elixirEchoServerStartLink
    , SupervisorHelpers.worker "ProtoHackers.EchoServer" (EchoServer.startLink {})
    , SupervisorHelpers.worker "ProtoHackers.ElixirPrimeServer" elixirPrimeServerStartLink
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

foreign import elixirEchoServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPrimeServerStartLink :: Effect (StartLinkResult Pid)
