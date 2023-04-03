module ProtoHackers.EchoServer
  ( startLink
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Erl.Atom as Atom
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Kernel.Inet (ConnectedSocket, PassiveSocket)
import Erl.Kernel.Inet as Inet
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Kernel.Tcp as Tcp
import Erl.Process.Raw as RawProcess
import Erl.Types (Timeout(..))
import Foreign as Foreign
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InfoFn, InitFn, InitResult(..), ServerSpec)
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import ProtoHackers.EchoServer.Types (Arguments, Message(..), ServerType', State, Pid)

serverName :: RegistryName ServerType'
serverName = "EchoServer" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  arguments # spec # GenServer.startLink

spec :: Arguments -> ServerSpec Unit Unit Message State
spec arguments =
  (arguments # init # GenServer.defaultSpec) { name = Just serverName, handleInfo = Just handleInfo }

init :: Arguments -> InitFn Unit Unit Message State
init {} = do
  let port = 4200
  maybeSocket <-
    { exit_on_close: false, reuseaddr: true }
      # Tcp.listenPassive (wrap port)
      # liftEffect
  case maybeSocket of
    Right socket -> do
      [ "Listening on port: ", show port ] # Array.fold # Console.log
      _timerId <- Timer.sendAfter (wrap 0.0) AcceptConnections
      { socket } # InitOk # pure
    Left error ->
      error # Foreign.unsafeToForeign # InitStop # pure

handleInfo :: InfoFn Unit Unit Message State
handleInfo AcceptConnections state = do
  _timerId <- Timer.sendAfter (wrap 0.0) AcceptConnections
  maybeSocket <- Tcp.acceptPassive state.socket (10_000.0 # wrap # Timeout) # liftEffect
  case maybeSocket of
    Right clientSocket -> do
      _spawnedPid <- clientSocket # handleClient # RawProcess.spawn # liftEffect
      pure unit

    Left _acceptError -> do
      pure unit

  state # GenServer.return # pure

handleClient :: TcpSocket PassiveSocket ConnectedSocket -> Effect Unit
handleClient clientSocket = do
  [ "Handling client: ", show clientSocket ] # Array.fold # Console.log
  maybeMessage <- receiveAll clientSocket
  traverse_ (Tcp.send clientSocket) maybeMessage
  Tcp.close clientSocket

receiveAll :: TcpSocket PassiveSocket ConnectedSocket -> Effect (Maybe IOData)
receiveAll clientSocket = do
  receiveAll' clientSocket IOData.empty

receiveAll' :: TcpSocket PassiveSocket ConnectedSocket -> IOData -> Effect (Maybe IOData)
receiveAll' clientSocket message = do
  maybeMessage <- Tcp.recv clientSocket 0 (100.0 # wrap # Timeout)
  case maybeMessage of
    Right message' -> do
      receiveAll' clientSocket (message' # IOData.fromBinary # IOData.append_ message)

    Left Inet.ActiveClosed -> do
      pure (Just message)

    Left other -> do
      Console.errorShow other
      pure Nothing
