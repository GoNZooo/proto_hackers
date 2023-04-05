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
import ProtoHackers.PriceServer as PriceServer
import ProtoHackers.PriceServer.Session.Supervisor as SessionSupervisor
import ProtoHackers.PrimeServer as PrimeServer
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
    , SupervisorHelpers.worker "ProtoHackers.PrimeServer" (PrimeServer.startLink {})
    , SupervisorHelpers.worker "ProtoHackers.ElixirPriceServer" elixirPriceServerStartLink
    , SupervisorHelpers.supervisor
        "ProtoHackers.ElixirPriceServer.Session.Supervisor"
        elixirPriceSessionSupervisorStartLink
    , SupervisorHelpers.supervisor
        "ProtoHackers.PriceServer.Session.Supervisor"
        SessionSupervisor.startLink
    , SupervisorHelpers.worker "ProtoHackers.PriceServer" (PriceServer.startLink {})
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

foreign import elixirEchoServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPrimeServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPriceServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPriceSessionSupervisorStartLink :: Effect (StartLinkResult Pid)
