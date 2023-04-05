-module(protoHackers_priceServer_session@foreign).

-export([parseRequest/1, meanPriceResponse/1]).

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
