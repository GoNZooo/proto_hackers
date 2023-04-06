-module(simpleServer@foreign).

-behavior(gen_server).

-export([startLinkBare_/2, startLink_/2, cast/2, call/2, init/1, handle_info/2,
         handle_cast/2, handle_call/3, serverLoop/3]).

% `name` is either `{nothing}` or `{just, Name}` where `Name` is
% `{local, Name}`, `{global, Name}`, or `{via, Module, Name}`.
startLinkBare_(StartArguments,
               #{init := Init,
                 handleInfo := HandleInfo,
                 name := {nothing}}) ->
  fun() ->
     % spawn a server loop and register it with the given name
     Pid = spawn_link(?MODULE, serverLoop, [StartArguments, Init, HandleInfo]),
     {right, Pid}
  end;
startLinkBare_(StartArguments,
               #{init := Init,
                 handleInfo := HandleInfo,
                 name := {just, Name}}) ->
  fun() ->
     % spawn a server loop and register it with the given name
     MaybePid = get_name(Name),
     case MaybePid of
       Pid when is_pid(Pid) -> {left, {alreadyStarted, Pid}};
       undefined ->
         Pid = spawn_link(?MODULE, serverLoop, [StartArguments, Init, HandleInfo]),
         RegistrationResult = try_register(Name, Pid),
         translateRegistrationResult(RegistrationResult, Pid)
     end
  end.

get_name({local, Name}) ->
  whereis(Name);
get_name({global, Name}) ->
  global:whereis_name(Name);
get_name({via, Module, Name}) ->
  Module:whereis(Name).

try_register({local, Name}, Pid) ->
  register(Name, Pid);
try_register({global, Name}, Pid) ->
  global:register_name(Name, Pid);
try_register({via, Module, Name}, Pid) ->
  Module:register(Name, Pid).

translateRegistrationResult({ok, Pid}, Pid) ->
  {right, Pid};
translateRegistrationResult({error, {already_registered, Pid}}, Pid) ->
  {left, {alreadyStarted, Pid}};
translateRegistrationResult(true, Pid) ->
  {right, Pid};
translateRegistrationResult(yes, Pid) ->
  {right, Pid};
translateRegistrationResult(false, _Pid) ->
  {left, {failed, unable_to_register}};
translateRegistrationResult(no, _Pid) ->
  {left, {failed, unable_to_register}}.

serverLoop(StartArguments, Init, HandleInfo) ->
  case (Init(StartArguments))() of
    {simpleInitOk, State} ->
      loop(State, HandleInfo);
    {simpleInitError, Foreign} ->
      exit({simpleInitError, Foreign})
  end.

loop(State, HandleInfo) ->
  receive
    {cast, F} ->
      case (F(State))() of
        {simpleNoReply, NewState} ->
          loop(NewState, HandleInfo);
        {simpleReply, _Reply, _NewState} ->
          throw({reply_not_allowed, {cast, F}});
        {simpleStop, Reason, _NewState} ->
          exit(translate_stop_reason(Reason))
      end;
    {call, F, From, Ref} ->
      case ((F(From))(State))() of
        {simpleReply, Reply, NewState} ->
          From ! {simpleReply, Ref, Reply},
          loop(NewState, HandleInfo);
        {simpleNoReply, _NewState} ->
          throw({reply_required, {call, F, From, Ref}});
        {simpleStop, Reason, _NewState} ->
          exit(translate_stop_reason(Reason))
      end;
    Message ->
      case ((HandleInfo(Message))(State))() of
        {simpleNoReply, NewState} ->
          loop(NewState, HandleInfo);
        {simpleReply, _Reply, _NewState} ->
          throw({reply_not_allowed, Message});
        {simpleStop, Reason, _NewState} ->
          exit(translate_stop_reason(Reason))
      end
  end.

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown}) ->
  shutdown;
translate_stop_reason({stopOther, Reason}) ->
  Reason.

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
