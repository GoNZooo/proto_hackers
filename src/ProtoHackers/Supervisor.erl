-module(protoHackers_supervisor@foreign).

-export([elixirEchoServerStartLink/0, elixirPrimeServerStartLink/0,
         elixirPriceServerStartLink/0, elixirPriceSessionSupervisorStartLink/0,
         elixirChatServerStartLink/0, elixirChatServerSessionSupervisorStartLink/0,
         elixirChatServerPresenceStartLink/0, pgStartLink/0, elixirKeyValueStoreStartLink/0,
         elixirChatProxyClientSupervisorStartLink/0, elixirChatProxyStartLink/0]).

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

elixirChatServerStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirChatServer':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirChatServerSessionSupervisorStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirChatServer.Session.Supervisor':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirChatServerPresenceStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirChatServer.Presence':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

pgStartLink() ->
  fun() ->
     case pg:start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirKeyValueStoreStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirKeyValueStore':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirChatProxyClientSupervisorStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirChatProxy.Client.Supervisor':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.

elixirChatProxyStartLink() ->
  fun() ->
     case 'Elixir.ProtoHackers.ElixirChatProxy':start_link() of
       {ok, Pid} -> {right, Pid};
       {error, Reason} -> {left, Reason}
     end
  end.
