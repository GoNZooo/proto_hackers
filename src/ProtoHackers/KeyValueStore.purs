module ProtoHackers.KeyValueStore
  ( startLink
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom as Atom
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Kernel.Inet (HostAddress, IpAddressUnion, Port)
import Erl.Kernel.Inet as HostAddress
import Erl.Kernel.Inet as IpAddress
import Erl.Kernel.Udp (UdpRecvData)
import Erl.Kernel.Udp as ReceiveError
import Erl.Kernel.Udp as Udp
import Erl.Kernel.Udp as UdpRecvData
import Erl.Process (ProcessM)
import Erl.Types (Timeout(..))
import Erl.Untagged.Union as Union
import Foreign as Foreign
import Logger as LogType
import Logger as Logger
import Pinto (RegistryName(..), StartLinkResult)
import ProtoHackers.KeyValueStore.Types
  ( Arguments
  , Command(..)
  , Continue
  , Message(..)
  , State
  , Stop
  , Pid
  )
import SimpleServer.GenServer (InitValue, ReturnValue, StopReason)
import SimpleServer.GenServer as SimpleServer
import SimpleServer.Types as StopReason
import Unsafe.Coerce as Coerce

version :: String
version = "ProtoHackers.KeyValueStore-1.0 (PureScript/purerl)"

serverName :: RegistryName Pid
serverName = "ProtoHackers.KeyValueStore" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  SimpleServer.startLink
    arguments
    { name: Just serverName, init, handleInfo, handleContinue, terminate }

init :: Arguments -> ProcessM Message (InitValue State Continue Stop)
init {} = do
  maybeSocket <- liftEffect $ Udp.openPassive (wrap 4200) { reuseaddr: true }
  case maybeSocket of
    Right socket -> do
      SimpleServer.sendSelf Read
      let message = "'ProtoHackers.KeyValueStore' listening on port 4200 (UDP)"
      { message } # Logger.info { domain: List.nil, type: LogType.Trace } # liftEffect
      { socket, store: Map.singleton "version" version } # SimpleServer.initOk # pure
    Left error -> do
      let message = "Failed to open UDP socket"
      { error, message } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      error # SimpleServer.initError # pure

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleInfo Read state = do
  SimpleServer.sendSelf Read

  maybeData <- liftEffect $ Udp.recv state.socket (50.0 # wrap # Timeout)
  case maybeData of
    Right recvData -> do
      let { host, port, data' } = getUdpInfo recvData
      case data' # Coerce.unsafeCoerce # parseCommand of
        Just (Query { key }) -> do
          let value = state.store # Map.lookup key # Maybe.fromMaybe ""
          (key <> "=" <> value)
            # IOData.fromString
            # Udp.send state.socket host port
            # void
            # liftEffect
          state # SimpleServer.noReply # pure
        Just (Insert { key: "version" }) -> do
          state # SimpleServer.noReply # pure
        Just (Insert { key, value }) -> do
          state { store = Map.insert key value state.store } # SimpleServer.noReply # pure
        Nothing -> do
          state # SimpleServer.noReply # pure

    Left ReceiveError.ReceiveTimeout ->
      state # SimpleServer.noReply # pure

    Left ReceiveError.ReceiveNotOwner ->
      state # SimpleServer.stop ("not_owner" # Foreign.unsafeToForeign # StopReason.StopOther) # pure

    Left (ReceiveError.ReceivePosix error) -> do
      let message = "Failed to receive UDP packet"
      { error, message } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      state
        # SimpleServer.stop ("posix_error" # Foreign.unsafeToForeign # StopReason.StopOther)
        # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleContinue _ state = do
  state # SimpleServer.noReply # pure

terminate :: StopReason Stop -> State -> ProcessM Message Unit
terminate _ _ = pure unit

getHost :: IpAddressUnion -> HostAddress
getHost =
  Union.case_
    # Union.on (\ipv4 -> HostAddress.Ip $ IpAddress.Ip4 ipv4)
    # Union.on (\ipv6 -> HostAddress.Ip $ IpAddress.Ip6 ipv6)

getUdpInfo :: UdpRecvData -> { host :: HostAddress, port :: Port, data' :: Binary }
getUdpInfo (UdpRecvData.Data hostUnion port data') = do
  let host = getHost hostUnion
  { host, port, data' }
getUdpInfo (UdpRecvData.DataAnc hostUnion port _anc data') = do
  let host = getHost hostUnion
  { host, port, data' }

foreign import parseCommand :: String -> Maybe Command
