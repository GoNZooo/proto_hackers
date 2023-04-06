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
import Erl.Process (Process, ProcessM)
import Erl.Process as Process
import Erl.Types (Timeout(..))
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
  )
import ProtoHackers.PriceServer.Session.Types as Request
import SimpleServer as SimpleServer
import SimpleServer.Types (InitValue, ReturnValue, StopReason(..))
import Unsafe.Coerce as UnsafeCoerce

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { init, handleInfo, name: Nothing }

init :: Arguments -> ProcessM Message (InitValue State)
init { socket } = do
  self' <- Process.self
  liftEffect $ Process.send self' ReadRequest
  pure $ SimpleServer.initOk { socket, prices: MapSet.empty }

foreign import sendSelf :: Message -> ProcessM Message Unit

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State)
handleInfo ReadRequest state = do
  maybeData <- liftEffect $ Tcp.recv state.socket 9 InfiniteTimeout
  case maybeData of
    Right data' -> do
      case parseRequest (UnsafeCoerce.unsafeCoerce data') of
        Right (Request.Insert insertData) -> do
          sendSelf ReadRequest
          let newPrices = handleInsert insertData state.prices
          state { prices = newPrices } # SimpleServer.noReply # pure
        Right (Request.Query queryData) -> do
          sendSelf ReadRequest
          state # handleQuery queryData # liftEffect
          state # SimpleServer.noReply # pure
        Left error -> do
          sendSelf ReadRequest
          let message = "Error parsing request"
          { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
          state # SimpleServer.noReply # pure
    Left ActiveError.ActiveTimeout -> do
      state # SimpleServer.noReply # pure
    Left ActiveError.ActiveClosed -> do
      state # SimpleServer.stop StopNormal # pure
    Left error -> do
      let message = "Error reading from client socket"
      { message, error } # Logger.error { domain: List.nil, type: LogType.Trace } # liftEffect
      state # SimpleServer.noReply # pure

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

foreign import parseRequest :: String -> Either InvalidRequest Request
foreign import meanPriceResponse :: Int -> IOData
foreign import mapList :: forall a b. (a -> b) -> List a -> List b
foreign import sumList :: forall a. List a -> a
