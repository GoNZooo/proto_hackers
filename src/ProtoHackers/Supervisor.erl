-module(protoHackers_supervisor@foreign).

-export([elixirEchoServerStartLink/0]).

elixirEchoServerStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirTcpEchoServer':start_link([]) of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.
