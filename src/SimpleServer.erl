-module(simpleServer@foreign).

-export([startLink_/2, cast/2, call/2, serverLoop/3]).

% `name` is either `{nothing}` or `{just, Name}` where `Name` is
% `{local, Name}`, `{global, Name}`, or `{via, Module, Name}`.
startLink_(StartArguments,
           #{init := Init,
             handleInfo := HandleInfo,
             name := {nothing}}) ->
  fun() ->
     % spawn a server loop and register it with the given name
     Pid = spawn_link(?MODULE, serverLoop, [StartArguments, Init, HandleInfo]),
     {right, Pid}
  end;
startLink_(StartArguments,
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

cast(Pid, F) ->
  fun() -> Pid ! {cast, F} end.

call(Pid, F) ->
  fun() ->
     Ref = make_ref(),
     From = self(),
     Pid ! {call, F, From, Ref},
     receive {simpleReply, Ref, Reply} -> Reply end
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

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown}) ->
  shutdown;
translate_stop_reason({stopOther, Reason}) ->
  Reason.
