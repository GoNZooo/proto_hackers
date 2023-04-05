-module(data_mapSet@foreign).

-export([empty/0, toList/1, insert/2]).

empty() ->
  'Elixir.MapSet':new().

toList(Set) ->
  'Elixir.MapSet':to_list(Set).

insert(Element, Set) ->
  'Elixir.MapSet':put(Set, Element).
