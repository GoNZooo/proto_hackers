module ProtoHackers.PriceServer.Session
  ( startLink
  , parseRequest
  ) where

import Prelude

import Data.Either (Either(..))
import Data.MapSet (MapSet)
import Data.MapSet as MapSet
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Kernel.Inet as ActiveError
import Erl.Kernel.Tcp as Tcp
import Erl.Process as Process
import Erl.Types (Timeout(..))
import Foreign (Foreign)
import Logger as LogType
import Logger as Logger
import Pinto (StartLinkResult)
import Pinto.GenServer (InfoFn, InitFn, InitResult(..), ServerSpec, ContinueFn)
import Pinto.GenServer as Action
import Pinto.GenServer as GenServer
import ProtoHackers.PriceServer.Session.Types
  ( Arguments
  , InvalidRequest
  , Message(..)
  , Pid
  , Request
  , State
  , PriceData
  )
import ProtoHackers.PriceServer.Session.Types as Request
import Unsafe.Coerce as UnsafeCoerce

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  arguments # spec # GenServer.startLink

spec :: Arguments -> ServerSpec Message Unit Message State
spec arguments = do
  (arguments # init # GenServer.defaultSpec)
    { handleInfo = Just handleInfo, handleContinue = Just handleContinue }

init :: Arguments -> InitFn Message Unit Message State
init { socket } = do
  InitOkContinue { socket, prices: MapSet.empty } ReadRequest # pure

handleContinue :: ContinueFn Message Unit Message State
handleContinue ReadRequest state = do
  maybeData <- liftEffect $ Tcp.recv state.socket 9 InfiniteTimeout
  case maybeData of
    Right data' -> do
      case parseRequest (UnsafeCoerce.unsafeCoerce data') of
        Right (Request.Insert insertData) -> do
          let newPrices = handleInsert insertData state.prices
          state { prices = newPrices }
            # GenServer.returnWithAction (Action.Continue ReadRequest)
            # pure
        Right (Request.Query queryData) -> do
          state # handleQuery queryData # liftEffect
          state # GenServer.returnWithAction (Action.Continue ReadRequest) # pure
        Left error -> do
          let message = "Error parsing request"
          { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
          state # GenServer.returnWithAction (Action.Continue ReadRequest) # pure
    Left ActiveError.ActiveClosed -> do
      state # GenServer.return # pure
    Left ActiveError.ActiveTimeout -> do
      state # GenServer.returnWithAction Action.StopNormal # pure
    Left error -> do
      let message = "Error reading from client socket"
      { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      state # GenServer.return # pure

handleInfo :: InfoFn Message Unit Message State
handleInfo ReadRequest state = do
  self' <- Process.self
  maybeData <- liftEffect $ Tcp.recv state.socket 9 InfiniteTimeout
  case maybeData of
    Right data' -> do
      case parseRequest (UnsafeCoerce.unsafeCoerce data') of
        Right (Request.Insert insertData) -> do
          liftEffect $ Process.send self' ReadRequest
          let newPrices = handleInsert insertData state.prices
          state { prices = newPrices } # GenServer.return # pure
        Right (Request.Query queryData) -> do
          liftEffect $ Process.send self' ReadRequest
          state # handleQuery queryData # liftEffect
          state # GenServer.return # pure
        Left error -> do
          liftEffect $ Process.send self' ReadRequest
          let message = "Error parsing request"
          { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
          state # GenServer.return # pure
    Left ActiveError.ActiveTimeout -> do
      state # GenServer.return # pure
    Left ActiveError.ActiveClosed -> do
      state # GenServer.returnWithAction Action.StopNormal # pure
    Left error -> do
      let message = "Error reading from client socket"
      { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      state # GenServer.return # pure

handleInsert :: PriceData -> MapSet PriceData -> MapSet PriceData
handleInsert { timestamp, price } prices = do
  MapSet.insert { timestamp, price } prices

handleQuery :: { minimumTimestamp :: Int, maximumTimestamp :: Int } -> State -> Effect Unit
handleQuery { minimumTimestamp, maximumTimestamp } { socket, prices } = do
  let
    pricesInRange =
      prices
        # MapSet.toList
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
