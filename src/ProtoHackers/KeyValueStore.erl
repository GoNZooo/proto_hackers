-module(protoHackers_keyValueStore@foreign).

-export([parseCommand/1]).

-define(MAXIMUM_COMMAND_SIZE, 1000).

-spec parseCommand(Binary) ->
                    {just, {insert, #{key := binary(), value := binary()}}} |
                    {just, {query, #{key := binary()}}} |
                    {nothing}
  when Binary :: binary().
parseCommand(Binary) when byte_size(Binary) < ?MAXIMUM_COMMAND_SIZE ->
  case binary:split(Binary, <<"=">>) of
    [Key] ->
      {just, {query, #{key => Key}}};
    [Key, Value] ->
      {just, {insert, #{key => Key, value => Value}}};
    _Other ->
      {nothing}
  end;
parseCommand(_Other) ->
  {nothing}.
