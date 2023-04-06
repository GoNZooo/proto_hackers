-module(simpleGenServer@foreign).

-export([startLink_/2, cast/2, call/2, init/1, handle_info/2, handle_cast/2,
         handle_call/3]).

startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             name := {nothing}}) ->
  fun() ->
     case gen_server:start_link(?MODULE, {StartArguments, Init, HandleInfo}, []) of
       {ok, Pid} -> {right, Pid};
       ignore -> {left, ignore};
       {error, {already_started, Pid}} -> {left, {alreadyStarted, Pid}};
       {error, Reason} -> {left, {failed, Reason}}
     end
  end;
startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             name := {just, Name}}) ->
  fun() ->
     case gen_server:start_link(Name, ?MODULE, {StartArguments, Init, HandleInfo}, []) of
       {ok, Pid} -> {right, Pid};
       ignore -> {left, ignore};
       {error, {already_started, Pid}} -> {left, {alreadyStarted, Pid}};
       {error, Reason} -> {left, {failed, Reason}}
     end
  end.

cast(Pid, F) ->
  fun() -> gen_server:cast(Pid, {cast, F}) end.

call(Pid, F) ->
  fun() -> gen_server:call(Pid, {call, F}) end.

init({StartArguments, Init, HandleInfo}) ->
  case (Init(StartArguments))() of
    {simpleInitOk, State} ->
      {ok, #{state => State, handleInfo => HandleInfo}};
    {simpleInitError, Foreign} ->
      {stop, Foreign}
  end.

handle_info(Message, #{state := State, handleInfo := HandleInfo}) ->
  case ((HandleInfo(Message))(State))() of
    {simpleNoReply, NewState} ->
      {noreply, #{state => NewState, handleInfo => HandleInfo}};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), #{state => NewState, handleInfo => HandleInfo}}
  end.

handle_cast({cast, F}, State) ->
  case (F(State))() of
    {simpleNoReply, NewState} ->
      {noreply, NewState};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), NewState}
  end.

handle_call(From, {call, F}, State) ->
  case ((F(From))(State))() of
    {simpleReply, Reply, NewState} ->
      {reply, Reply, NewState};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), NewState}
  end.

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown}) ->
  shutdown;
translate_stop_reason({stopOther, Reason}) ->
  Reason.
