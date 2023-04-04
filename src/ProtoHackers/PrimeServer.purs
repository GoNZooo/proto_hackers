module ProtoHackers.PrimeServer
  ( startLink
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom as Atom
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List as List
import Erl.Kernel.Inet (ConnectedSocket, PassiveSocket)
import Erl.Kernel.Inet as ActiveError
import Erl.Kernel.Inet as Inet
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Kernel.Tcp as AcceptError
import Erl.Kernel.Tcp as SocketPacket
import Erl.Kernel.Tcp as Tcp
import Erl.Process.Raw as RawProcess
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Logger as LogType
import Logger as Logger
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.GenServer (CallFn, CastFn, InfoFn, InitFn, InitResult(..), ServerSpec)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import ProtoHackers.PrimeServer.Types (Arguments, Message(..), ServerType', State, Pid)
import Simple.JSON as Json
import Unsafe.Coerce as UnsafeCoerce

type PrimeRequest = { method :: String, number :: Number }

serverName :: RegistryName ServerType'
serverName = "ProtoHackers.PrimeServer" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  arguments # spec # GenServer.startLink

spec :: Arguments -> ServerSpec Unit Unit Message State
spec arguments =
  (arguments # init # GenServer.defaultSpec) { name = Just serverName, handleInfo = Just handleInfo }

init :: Arguments -> InitFn Unit Unit Message State
init {} = do
  maybeSocket <-
    { exit_on_close: false, reuseaddr: true, buffer: Just buffer, packet: Just SocketPacket.Line }
      # Tcp.listenPassive (wrap 4202)
      # liftEffect
  case maybeSocket of
    Right socket -> do
      _timerId <- Timer.sendAfter (wrap 0.0) AcceptClients
      let message = "'ProtoHackers.PrimeServer' listening on port 4202"
      { message } # Logger.info { domain: List.nil, type: LogType.Event } # liftEffect
      { socket } # InitOk # pure
    Left error ->
      error # Foreign.unsafeToForeign # InitStop # pure

handleInfo :: InfoFn Unit Unit Message State
handleInfo AcceptClients state = do
  _timerId <- Timer.sendAfter (wrap 0.0) AcceptClients
  maybeClient <- Tcp.acceptPassive state.socket (420.0 # wrap # Timeout) # liftEffect

  case maybeClient of
    Right socket -> do
      handleClient socket # RawProcess.spawn # void # liftEffect

    Left AcceptError.AcceptTimeout -> do
      pure unit

    Left error -> do
      { message: "Failed to accept client", error }
        # Logger.error { domain: List.nil, type: LogType.Event }
        # liftEffect

  state # GenServer.return # pure

handleClient :: TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
handleClient socket = do
  maybeLine <- Tcp.recv socket 0 (420.0 # wrap # Timeout) # liftEffect

  case maybeLine of
    Right line -> do
      let (asString :: String) = UnsafeCoerce.unsafeCoerce line
      case Json.readJSON asString of
        Right ({ method: "isPrime", number } :: PrimeRequest) -> do
          case Int.fromNumber number of
            Just i -> do
              let response = { method: "isPrime", prime: isPrime i }
              response # Json.writeJSON # (_ <> "\n") # IOData.fromString # Tcp.send socket # void
            Nothing -> do
              let response = { method: "isPrime", prime: false }
              response # Json.writeJSON # (_ <> "\n") # IOData.fromString # Tcp.send socket # void

          handleClient socket

        _other -> do
          "malformed request\n" # IOData.fromString # Tcp.send socket # void
          Tcp.close socket

    Left ActiveError.ActiveClosed -> do
      Tcp.close socket

    Left error -> do
      Logger.error
        { domain: List.nil, type: LogType.Event }
        { message: "Failed to receive request", error }
      Tcp.close socket

buffer :: Int
buffer = 1024 * 50

foreign import isPrime :: Int -> Boolean
