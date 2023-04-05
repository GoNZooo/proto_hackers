-module(simpleServer@foreign).

-behavior(gen_server).

-export([startLink_/2, init/1, handle_info/2, handle_cast/2, handle_call/3]).

startLink_(StartArguments, #{init := Init, handleInfo := HandleInfo}) ->
  fun() ->
     Result = gen_server:start_link(?MODULE, {StartArguments, Init, HandleInfo}, []),
     case Result of
       {ok, Pid} -> {right, Pid};
       ignore -> {left, ignore};
       {error, {already_started, Pid}} -> {left, {alreadyStarted, Pid}};
       {error, Reason} -> {left, {failed, Reason}}
     end
  end.

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
    {simpleStop, NewState} ->
      {stop, normal, #{state => NewState, handleInfo => HandleInfo}}
  end.

handle_cast(_, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.
