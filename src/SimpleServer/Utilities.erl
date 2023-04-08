-module(simpleServer_utilities@foreign).

-export([sendSelf/1]).

sendSelf(Message) ->
  fun() -> self() ! Message end.

