-module(protoHackers_primeServer@foreign).

-export([isPrime/1]).

isPrime(N) ->
  'Elixir.ProtoHackers.Prime':'prime?'(N).
