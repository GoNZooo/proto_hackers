module ProtoHackers.ChatServer
  ( startLink
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom as Atom
import Erl.Data.List as List
import Erl.Kernel.Tcp as SocketPacket
import Erl.Kernel.Tcp as Tcp
import Erl.Process (ProcessM)
import Erl.Types (Timeout(..))
import Logger as LogType
import Logger as Logger
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.ChatServer.Client.Supervisor as ClientSupervisor
import ProtoHackers.ChatServer.Types (Arguments, Continue, Message(..), Pid, State, Stop)
import SimpleServer.GenServer (InitValue, ReturnValue, StopReason)
import SimpleServer.GenServer as SimpleServer

serverName :: RegistryName Pid
serverName = "ProtoHackers.ChatServer" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  SimpleServer.startLink
    arguments
    { name: Just serverName, init, handleInfo, handleContinue, terminate }

init :: Arguments -> ProcessM Message (InitValue State Continue Stop)
init {} = do
  maybeSocket <-
    { exit_on_close: false, reuseaddr: true, packet: Just SocketPacket.Line }
      # Tcp.listenPassive (wrap 4206)
      # liftEffect
  case maybeSocket of
    Right socket -> do
      let message = "'ProtoHackers.ChatServer' Listening on port 4206"
      { message } # Logger.info { domain: List.nil, type: LogType.Trace } # liftEffect
      SimpleServer.sendSelf Accept
      { socket } # SimpleServer.initOk # pure
    Left error -> error # SimpleServer.initError # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleContinue _ state = state # SimpleServer.noReply # pure

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleInfo Accept state = do
  SimpleServer.sendSelf Accept
  maybeSocket <- Tcp.acceptPassive state.socket InfiniteTimeout # liftEffect
  case maybeSocket of
    Right clientSocket -> do
      { socket: clientSocket } # ClientSupervisor.startChild # void # liftEffect
      state # SimpleServer.noReply # pure
    Left error -> do
      let message = "Error accepting client socket"
      { message, error }
        # Logger.error { domain: List.nil, type: LogType.Trace }
        # liftEffect
      state # SimpleServer.noReply # pure

terminate :: StopReason Stop -> State -> ProcessM Message Unit
terminate _ _ = pure unit
