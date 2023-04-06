-module(protoHackers_priceServer_session@foreign).

-export([parseRequest/1, meanPriceResponse/1, mapList/2, sumList/1, selfPid/0, recv/2,
         sendSelf/1]).

parseRequest(<<"I", Timestamp:32/big-signed-integer, Price:32/big-signed-integer>>) ->
  {right, {insert, #{timestamp => Timestamp, price => Price}}};
parseRequest(<<"Q",
               MinimumTime:32/big-signed-integer,
               MaximumTime:32/big-signed-integer>>) ->
  {right, {query, #{minimumTimestamp => MinimumTime, maximumTimestamp => MaximumTime}}};
parseRequest(Other) ->
  {left, #{value => Other}}.

meanPriceResponse(MeanPrice) ->
  <<MeanPrice:32/big-signed-integer>>.

mapList(F, List) ->
  lists:map(F, List).

sumList(List) ->
  lists:sum(List).

selfPid() ->
  fun() -> self() end.

sendSelf(Message) ->
  fun() -> self() ! Message end.

recv(Socket, Length) ->
  fun() ->
     case gen_tcp:recv(Socket, Length) of
       {ok, Data} -> {right, Data};
       {error, closed} -> {left, {recvErrorClosed}};
       {error, timeout} -> {left, {recvErrorTimeout}};
       {error, Reason} -> {left, {recvErrorOther, Reason}}
     end
  end.
