module ProtoHackers.PriceServer.Session
  ( startLink
  , parseRequest
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Set (Set)
import Erl.Data.Set as Set
import Erl.Kernel.Inet (ConnectedSocket, PassiveSocket)
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Kernel.Tcp as Tcp
import Erl.Process (ProcessM)
import Erl.Process as Process
import Foreign (Foreign)
import Logger as LogType
import Logger as Logger
import Pinto (StartLinkResult)
import ProtoHackers.PriceServer.Session.Types
  ( Arguments
  , InvalidRequest
  , Message(..)
  , PriceData
  , Request
  , State
  , Continue
  )
import ProtoHackers.PriceServer.Session.Types as Request
import SimpleServer.GenServer (InitValue, ReturnValue, ServerPid, StopReason(..))
import SimpleServer.GenServer as SimpleServer
import Unsafe.Coerce as UnsafeCoerce

startLink :: Arguments -> Effect (StartLinkResult (ServerPid Message State Continue))
startLink arguments = do
  SimpleServer.startLink arguments { init, handleInfo, name: Nothing, handleContinue }

init :: Arguments -> ProcessM Message (InitValue State Continue)
init { socket } = do
  self' <- Process.self
  liftEffect $ Process.send self' ReadRequest
  pure $ SimpleServer.initOk { socket, prices: Set.empty }

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue)
handleInfo ReadRequest state = do
  maybeData <- liftEffect $ recv state.socket 9
  case maybeData of
    Right data' -> do
      case parseRequest (UnsafeCoerce.unsafeCoerce data') of
        Right (Request.Insert insertData) -> do
          SimpleServer.sendSelf ReadRequest
          let newPrices = handleInsert insertData state.prices
          state { prices = newPrices } # SimpleServer.noReply # pure
        Right (Request.Query queryData) -> do
          SimpleServer.sendSelf ReadRequest
          state # handleQuery queryData # liftEffect
          state # SimpleServer.noReply # pure
        Left error -> do
          SimpleServer.sendSelf ReadRequest
          let message = "Error parsing request"
          { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
          state # SimpleServer.noReply # pure
    Left RecvErrorTimeout -> do
      state # SimpleServer.noReply # pure
    Left RecvErrorClosed -> do
      state # SimpleServer.stop StopNormal # pure
    Left (RecvErrorOther error) -> do
      let message = "Error reading from client socket"
      { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      state # SimpleServer.noReply # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue)
handleContinue _ state = do
  state # SimpleServer.noReply # pure

handleInsert :: PriceData -> Set PriceData -> Set PriceData
handleInsert { timestamp, price } prices = do
  Set.insert { timestamp, price } prices

handleQuery :: { minimumTimestamp :: Int, maximumTimestamp :: Int } -> State -> Effect Unit
handleQuery { minimumTimestamp, maximumTimestamp } { socket, prices } = do
  let
    pricesInRange =
      prices
        # Set.toList
        # List.filter
            (\{ timestamp } -> minimumTimestamp <= timestamp && timestamp <= maximumTimestamp)
    meanPrice =
      if List.null pricesInRange then 0
      else pricesInRange
        # List.fromFoldable
        # mapList (\{ price } -> price)
        # sumList
        # (_ / List.length pricesInRange)
  meanPrice # meanPriceResponse # Tcp.send socket # void

data RecvError
  = RecvErrorClosed
  | RecvErrorTimeout
  | RecvErrorOther Foreign

foreign import parseRequest :: String -> Either InvalidRequest Request
foreign import meanPriceResponse :: Int -> IOData
foreign import mapList :: forall a b. (a -> b) -> List a -> List b
foreign import sumList :: forall a. List a -> a
foreign import recv
  :: TcpSocket PassiveSocket ConnectedSocket -> Int -> Effect (Either RecvError Binary)
