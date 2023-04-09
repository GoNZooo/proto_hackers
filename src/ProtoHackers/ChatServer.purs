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
import Erl.Process (Process, ProcessM)
import Erl.Types (Timeout(..))
import Logger as LogType
import Logger as Logger
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.ChatServer.Client.Supervisor as ClientSupervisor
import ProtoHackers.ChatServer.Types (Arguments, Message(..), State)
import SimpleServer.GenServer (InitValue, ReturnValue, ServerPid)
import SimpleServer.GenServer as SimpleServer

serverName :: RegistryName (ServerPid Message State)
serverName = "ProtoHackers.ChatServer" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { init, handleInfo, name: Just serverName }

init :: Arguments -> ProcessM Message (InitValue State)
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

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State)
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
