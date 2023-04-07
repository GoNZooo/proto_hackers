-module(simpleGenServer@foreign).

-export([startLink_/2, cast/2, call/2, init/1, handle_info/2, handle_cast/2,
         handle_call/3]).

-record(state, {state :: term(), handleInfo :: handle_info_function()}).

-type handle_info_function() :: fun((term(), term()) -> handle_info_return()).
-type handle_info_return() :: {simpleNoReply, term()} | {simpleStop, term(), term()}.

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
      {ok, #state{state = State, handleInfo = HandleInfo}};
    {simpleInitError, Foreign} ->
      {stop, Foreign}
  end.

handle_info(Message, #state{state = State, handleInfo = HandleInfo} = ServerState) ->
  case ((HandleInfo(Message))(State))() of
    {simpleNoReply, NewState} ->
      {noreply, ServerState#state{state = NewState}};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), ServerState#state{state = NewState}}
  end.

handle_cast({cast, F}, #state{state = State} = ServerState) ->
  case (F(State))() of
    {simpleNoReply, NewState} ->
      {noreply, ServerState#state{state = NewState}};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), ServerState#state{state = NewState}}
  end.

handle_call(From, {call, F}, #state{state = State} = ServerState) ->
  case ((F(From))(State))() of
    {simpleReply, Reply, NewState} ->
      {reply, Reply, ServerState#state{state = NewState}};
    {simpleStop, Reason, NewState} ->
      {stop, translate_stop_reason(Reason), ServerState#state{state = NewState}}
  end.

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown}) ->
  shutdown;
translate_stop_reason({stopOther, Reason}) ->
  Reason.
