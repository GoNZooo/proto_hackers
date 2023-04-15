module ProtoHackers.ChatServer.Client
  ( startLink
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Maybe as MaybePartial
import Data.Newtype (wrap)
import Data.String (CodePoint)
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Data.Binary.IOData as IOData
import Erl.Kernel.Erlang as Erlang
import Erl.Kernel.Inet as ActiveError
import Erl.Kernel.Tcp as SocketPacket
import Erl.Kernel.Tcp as Tcp
import Erl.Process (ProcessM)
import Erl.Types (Timeout(..))
import Partial.Unsafe as Partial
import Pinto (StartLinkResult)
import ProtoHackers.ChatServer.Client.Types (Arguments, Continue, Message(..), Pid, State, Stop)
import ProtoHackers.ChatServer.Presence as Presence
import ProtoHackers.ChatServer.Presence.Bus (UserEvent(..))
import ProtoHackers.ChatServer.Presence.Bus as PresenceBus
import SimpleServer.GenServer (InitValue, ReturnValue, StopReason(..))
import SimpleServer.GenServer as SimpleServer
import Unsafe.Coerce as UnsafeCoerce

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  SimpleServer.startLink arguments { name: Nothing, init, handleInfo, handleContinue, terminate }

init :: Arguments -> ProcessM Message (InitValue State Continue Stop)
init { socket } = do
  ref <- liftEffect Erlang.makeRef
  SimpleServer.sendSelf ReadUsername
  { packet: SocketPacket.Line } # Tcp.setopts socket # void # liftEffect

  "Welcome! What is your username?\n" # IOData.fromString # Tcp.send socket # void # liftEffect

  { socket, username: Nothing, ref } # SimpleServer.initOk # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleContinue _ state = state # SimpleServer.noReply # pure

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleInfo ReadUsername state = do
  maybeData <- liftEffect $ Tcp.recv state.socket 0 (500.0 # wrap # Timeout)
  case maybeData of
    Right data' -> do
      let username = data' # UnsafeCoerce.unsafeCoerce # String.trim
      if isValidUsername username then do
        users <- liftEffect Presence.getUsers
        (UserJoined { ref: state.ref, username }) # PresenceBus.publish # liftEffect
        let
          userListMessage =
            users
              # Array.fromFoldable
              # Array.intercalate ", "
              # (\s -> "* Users: " <> s <> "\n")
              # IOData.fromString
        Tcp.send state.socket userListMessage # void # liftEffect
        self <- SimpleServer.self
        liftEffect $ PresenceBus.subscribe self PresenceEvent
        SimpleServer.sendSelf ReadChatMessage
        state { username = Just username } # SimpleServer.noReply # pure
      else do
        state.socket # Tcp.close # liftEffect
        state # SimpleServer.noReply # pure
    Left ActiveError.ActiveClosed -> do
      state # SimpleServer.noReply # pure
    Left ActiveError.ActiveTimeout -> do
      SimpleServer.sendSelf ReadUsername
      state # SimpleServer.noReply # pure
    Left (ActiveError.ActivePosix _error) -> do
      state # SimpleServer.noReply # pure
handleInfo ReadChatMessage state = do
  SimpleServer.sendSelf ReadChatMessage
  maybeData <- liftEffect $ Tcp.recv state.socket 0 (10.0 # wrap # Timeout)
  let username = Partial.unsafePartial MaybePartial.fromJust state.username
  case maybeData of
    Right data' -> do
      let message = data' # UnsafeCoerce.unsafeCoerce # String.trim
      (UserSentMessage { ref: state.ref, username, message })
        # PresenceBus.publish
        # liftEffect
      state # SimpleServer.noReply # pure
    Left ActiveError.ActiveClosed -> do
      (UserLeft { ref: state.ref, username })
        # PresenceBus.publish
        # liftEffect
      state # SimpleServer.stop StopNormal # pure
    Left ActiveError.ActiveTimeout -> do
      state # SimpleServer.noReply # pure
    Left (ActiveError.ActivePosix _error) -> do
      state # SimpleServer.noReply # pure
handleInfo (PresenceEvent (UserJoined { ref, username })) state@{ ref: ourRef } = do
  when (ref /= ourRef) do
    let message = "* " <> username <> " has joined\n"
    message # IOData.fromString # Tcp.send state.socket # void # liftEffect
  state # SimpleServer.noReply # pure
handleInfo (PresenceEvent (UserLeft { ref, username })) state@{ ref: ourRef } = do
  when (ref /= ourRef) do
    let message = "* " <> username <> " has left\n"
    message # IOData.fromString # Tcp.send state.socket # void # liftEffect
  state # SimpleServer.noReply # pure
handleInfo (PresenceEvent (UserSentMessage { ref, username, message })) state@{ ref: ourRef } = do
  when (ref /= ourRef) do
    [ "[", username, "] ", message, "\n" ]
      # Array.fold
      # IOData.fromString
      # Tcp.send state.socket
      # void
      # liftEffect
  state # SimpleServer.noReply # pure

terminate :: StopReason Stop -> State -> ProcessM Message Unit
terminate _ _ = pure unit

isValidUsername :: String -> Boolean
isValidUsername username =
  String.length username > 0 &&
    (username # String.toCodePointArray # Array.all isValidUsernameCharacter)

isValidUsernameCharacter :: CodePoint -> Boolean
isValidUsernameCharacter codePoint = Array.elem codePoint validCodePoints

validCodePoints :: Array CodePoint
validCodePoints =
  [ Enum.enumFromTo 'a' 'z', Enum.enumFromTo 'A' 'Z', Enum.enumFromTo '0' '9' ]
    # Array.fold
    # map String.codePointFromChar
