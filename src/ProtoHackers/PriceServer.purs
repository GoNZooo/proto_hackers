module ProtoHackers.PriceServer
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
import Erl.Kernel.Tcp as AcceptError
import Erl.Kernel.Tcp as Tcp
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Logger as LogType
import Logger as Logger
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InfoFn, InitFn, InitResult(..), ServerSpec)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import ProtoHackers.PriceServer.Session.Supervisor as SessionSupervisor
import ProtoHackers.PriceServer.Types (Arguments, Message(..), Pid, ServerType', State)

serverName :: RegistryName ServerType'
serverName = "ProtoHackers.PriceServer" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  arguments # spec # GenServer.startLink

spec :: Arguments -> ServerSpec Unit Unit Message State
spec arguments =
  (arguments # init # GenServer.defaultSpec) { name = Just serverName, handleInfo = Just handleInfo }

init :: Arguments -> InitFn Unit Unit Message State
init {} = do
  { message: "ProtoHackers.PriceServer listening on port 4204" }
    # Logger.info { domain: List.nil, type: LogType.Trace }
    # liftEffect
  maybeSocket <-
    { exit_on_close: false, reuseaddr: true }
      # Tcp.listenPassive (wrap 4204)
      # liftEffect
  case maybeSocket of
    Right socket -> do
      _timerId <- Timer.sendAfter (wrap 0.0) AcceptClients
      { socket } # InitOk # pure
    Left error -> do
      error # Foreign.unsafeToForeign # InitStop # pure

handleInfo :: InfoFn Unit Unit Message State
handleInfo AcceptClients state = do
  _timerId <- Timer.sendAfter (wrap 0.0) AcceptClients

  maybeSocket <- 420.0 # wrap # Timeout # Tcp.acceptPassive state.socket # liftEffect
  case maybeSocket of
    Right clientSocket -> do
      { socket: clientSocket } # SessionSupervisor.startChild # void # liftEffect
    Left AcceptError.AcceptClosed -> do
      pure unit
    Left AcceptError.AcceptTimeout -> do
      pure unit
    Left error -> do
      let message = "Error accepting client"
      { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect

  state # GenServer.return # pure