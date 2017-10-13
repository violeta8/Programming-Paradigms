%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides the implementation of a logging API that acts as a backend to the
%%% logging facility provided by the log.hrl macros.
%%% ----------------------------------------------------------------------------
-module(log).
-include("log.hrl").

%%% Logger API exports.
-export([write/4, write/5]).


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Writes the log string to the output.
%% write(LogLevelStr, Module, Line, Format) where:
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "ERROR"), see log.hrl.
%%   * Module::atom() is the module name performing the log.
%%   * Line::integer() is the line number where the log is performed.
%%   * Format::string() is the standard format string used by io:format/1-3
%%     functions.
%% Returns: the no-op ok.
%% -----------------------------------------------------------------------------
write(LogLevelStr, Module, Line, Format) ->
  write(LogLevelStr, Module, Line, Format, []).

%% -----------------------------------------------------------------------------
%% Writes the log string to the output.
%% write(LogLevelStr, Module, Line, Format) where:
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "ERROR"), see log.hrl.
%%   * Module::atom() is the module name performing the log.
%%   * Line::integer() is the line number where the log is performed.
%%   * Format::string() is the standard format string used by io:format/1-3
%%     functions.
%%   * Params::list() is the list of parameters that are used by the Format
%%     string.
%% Returns: the no-op ok.
%% -----------------------------------------------------------------------------
write(LogLevelStr, Module, Line, Format, Params) ->
  case can_log(?log_level, LogLevelStr) of
    true ->
      io:fwrite(user, "[~s - ~p - ~p:~p] - ~s~n",
        [LogLevelStr, self(), Module, Line, io_lib:format(Format, Params)]);
    false -> ok
  end.


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Determines whether a log message can be output, given the specified trace
%% log level and log level string (see log.hrl).
%% can_log(LogLevel, LogLevelStr) where:
%%   * LogLevel::integer() is the trace level from 1 to 4 (1 = TRACE, 2 = DEBUG,
%%     3 = INFO, 4 = ERROR).
%%   * LogLevelStr::string() is the log string output next to each log ("TRACE",
%%     "DEBUG, "INFO", "ERROR").
%% Returns: true if the log statement can be output.
%%          | false if the log statement cannot be output.
%% -----------------------------------------------------------------------------
can_log(?trace_level, ?trace_str) ->
  true;
can_log(?trace_level, ?debug_str) ->
  true;
can_log(?trace_level, ?info_str) ->
  true;
can_log(?trace_level, ?error_str) ->
  true;

can_log(?debug_level, ?debug_str) ->
  true;
can_log(?debug_level, ?info_str) ->
  true;
can_log(?debug_level, ?error_str) ->
  true;

can_log(?info_level, ?info_str) ->
  true;
can_log(?info_level, ?error_str) ->
  true;

can_log(?error_level, ?error_str) ->
  true;
can_log(_, _) -> false.
