%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% Created: 20 Sep 2017
%%%-------------------------------------------------------------------
-module(log).
-include("log.hrl").
-export([write/4, write/5]). % Logger API.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API.                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Writes the log string to the output.
write(Level, Module, Line, Format) -> write(Level, Module, Line, Format, []).
write(Level, Module, Line, Format, Params) ->
  case can_log(?log_level, Level) of
    true ->
      io:fwrite(user, "[~s - ~p - ~p:~p] - ~s~n",
        [Level, self(), Module, Line, io_lib:format(Format, Params)]);
    false -> ok
  end.

%% Determines whether log messages can be produced, given the specified trace
%% level and trace string.
can_log(?trace_level, ?trace_str) -> true;
can_log(?trace_level, ?debug_str) -> true;
can_log(?trace_level, ?info_str) -> true;
can_log(?trace_level, ?error_str) -> true;

can_log(?debug_level, ?debug_str) -> true;
can_log(?debug_level, ?info_str) -> true;
can_log(?debug_level, ?error_str) -> true;

can_log(?info_level, ?info_str) -> true;
can_log(?info_level, ?error_str) -> true;

can_log(?error_level, ?error_str) -> true;
can_log(_, _) -> false.
