module ProtoHackers.PriceServer.Session
  ( startLink
  , parseRequest
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List as List
import Erl.Kernel.Inet as ActiveError
import Erl.Kernel.Tcp as Tcp
import Erl.Types (Timeout(..))
import Logger as LogType
import Logger as Logger
import Pinto (StartLinkResult)
import Pinto.GenServer (InfoFn, InitFn, InitResult(..), ServerSpec)
import Pinto.GenServer as Action
import Pinto.GenServer as GenServer
import Pinto.Timer as Timer
import ProtoHackers.PriceServer.Session.Types
  ( Arguments
  , InvalidRequest
  , Message(..)
  , Pid
  , PriceData
  , Request
  , State
  )
import ProtoHackers.PriceServer.Session.Types as Request
import Unsafe.Coerce as UnsafeCoerce

startLink :: Arguments -> Effect (StartLinkResult Pid)
startLink arguments = do
  arguments # spec # GenServer.startLink

spec :: Arguments -> ServerSpec Unit Unit Message State
spec arguments = do
  (arguments # init # GenServer.defaultSpec) { handleInfo = Just handleInfo }

init :: Arguments -> InitFn Unit Unit Message State
init { socket } = do
  _timerId <- Timer.sendAfter (wrap 0.0) ReadRequest
  { socket, prices: Set.empty } # InitOk # pure

handleInfo :: InfoFn Unit Unit Message State
handleInfo ReadRequest state = do
  maybeData <- liftEffect $ Tcp.recv state.socket 9 (420.0 # wrap # Timeout)
  case maybeData of
    Right data' -> do
      case data' # UnsafeCoerce.unsafeCoerce # parseRequest of
        Right (Request.Insert insertData) -> do
          _timerId <- Timer.sendAfter (wrap 0.0) ReadRequest
          newState <- state # handleInsert insertData # liftEffect
          newState # GenServer.return # pure
        Right (Request.Query queryData) -> do
          _timerId <- Timer.sendAfter (wrap 0.0) ReadRequest
          state # handleQuery queryData # liftEffect
          state # GenServer.return # pure
        Left error -> do
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

handleInsert :: PriceData -> State -> Effect State
handleInsert { timestamp, price } state@{ prices } = do
  pure $ state { prices = Set.insert { timestamp, price } prices }

handleQuery :: { minimumTimestamp :: Int, maximumTimestamp :: Int } -> State -> Effect Unit
handleQuery { minimumTimestamp, maximumTimestamp } { socket, prices } = do
  let
    pricesInRange =
      Set.filter
        (\{ timestamp } -> minimumTimestamp <= timestamp && timestamp <= maximumTimestamp)
        prices
    meanPrice =
      if Set.isEmpty pricesInRange then 0
      else pricesInRange
        # List.fromFoldable
        # map (\{ price } -> price)
        # sum
        # (_ / Set.size pricesInRange)
  meanPrice # meanPriceResponse # Tcp.send socket # void

foreign import parseRequest :: String -> Either InvalidRequest Request
foreign import meanPriceResponse :: Int -> IOData
