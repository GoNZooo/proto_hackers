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
import ProtoHackers.ChatServer as ChatServer
import ProtoHackers.ChatServer.Client.Supervisor as ChatSessionSupervisor
import ProtoHackers.ChatServer.Presence as Presence
import ProtoHackers.EchoServer as EchoServer
import ProtoHackers.KeyValueStore as KeyValueStore
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
    [ SupervisorHelpers.worker "PgWorker" pgStartLink
    , SupervisorHelpers.worker "ProtoHackers.TcpEchoServer" elixirEchoServerStartLink
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
    , SupervisorHelpers.worker "ProtoHackers.ElixirChatServer.Presence"
        elixirChatServerPresenceStartLink
    , SupervisorHelpers.worker "ProtoHackers.ChatServer.Presence" (Presence.startLink {})
    , SupervisorHelpers.worker "ProtoHackers.ElixirChatServer" elixirChatServerStartLink
    , SupervisorHelpers.supervisor
        "ProtoHackers.ElixirChatServer.Session.Supervisor"
        elixirChatServerSessionSupervisorStartLink
    , SupervisorHelpers.supervisor "ProtoHackers.ChatServer.Session.Supervisor"
        ChatSessionSupervisor.startLink
    , SupervisorHelpers.worker "ProtoHackers.ChatServer" (ChatServer.startLink {})
    , SupervisorHelpers.worker "ProtoHackers.ElixirKeyValueStore" elixirKeyValueStoreStartLink
    , SupervisorHelpers.worker "ProtoHackers.KeyValueStore" (KeyValueStore.startLink {})
    , SupervisorHelpers.supervisor "ProtoHackers.ElixirChatProxy.Client.Supervisor"
        elixirChatProxyClientSupervisorStartLink
    , SupervisorHelpers.worker "ProtoHackers.ElixirChatProxy" elixirChatProxyStartLink
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

foreign import elixirEchoServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPrimeServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPriceServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirPriceSessionSupervisorStartLink :: Effect (StartLinkResult Pid)
foreign import elixirChatServerStartLink :: Effect (StartLinkResult Pid)
foreign import elixirChatServerSessionSupervisorStartLink :: Effect (StartLinkResult Pid)
foreign import elixirChatServerPresenceStartLink :: Effect (StartLinkResult Pid)
foreign import pgStartLink :: Effect (StartLinkResult Pid)
foreign import elixirKeyValueStoreStartLink :: Effect (StartLinkResult Pid)
foreign import elixirChatProxyClientSupervisorStartLink :: Effect (StartLinkResult Pid)
foreign import elixirChatProxyStartLink :: Effect (StartLinkResult Pid)
