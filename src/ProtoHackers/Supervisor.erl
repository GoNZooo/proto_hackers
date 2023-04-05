-module(protoHackers_supervisor@foreign).

-export([elixirEchoServerStartLink/0, elixirPrimeServerStartLink/0,
         elixirPriceServerStartLink/0, elixirPriceSessionSupervisorStartLink/0]).

elixirEchoServerStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirTcpEchoServer':start_link([]) of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirPrimeServerStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirPrimeServer':start_link([]) of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirPriceServerStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirPriceServer':start_link([]) of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirPriceSessionSupervisorStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirPriceServer.Session.Supervisor':start_link([]) of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.
