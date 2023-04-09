-module(simple_server_utilities).

-export([translate_process_reference/1, translate_stop_reason/1]).


translate_process_reference({pidReference, Pid}) ->
  Pid;
translate_process_reference({nameReference, {local, Name}}) ->
  Name;
translate_process_reference({nameReference, Name}) ->
  Name.

translate_stop_reason({stopNormal}) ->
  normal;
translate_stop_reason({stopShutdown}) ->
  shutdown;
translate_stop_reason({stopOther, Reason}) ->
  Reason.
