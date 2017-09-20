%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% Created: 20 Sep 2017
%%%-------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client configuration macros.                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Comment out to disable logging entirely.
-define(log, log).

%% Controls the logging level. Log statements are output if the specified number
%% (must be between 1 and 4 inclusive) is equal or greater to the logging
%% statement being output. Each logging level is assigned a number as follows:
%% TRACE = 1
%% DEBUG = 2
%% INFO = 3
%% ERROR = 4
%%
%% For example, if the statement being output is DEBUG and the logging level is
%% 1 (TRACE), then the log is written (DEBUG is greater than TRACE).
%% Another example: if the log statement being output is INFO but the level is
%% 4 (ERROR), then the TRACE log is not written (TRACE is smaller than ERROR).
%% To be able to see the log, one would have to set the log level to any one of
%% 1 (TRACE), 2 (DEBUG) or 3 (TRACE).
%%
-ifndef(log_level).
-define(log_level, 1).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal macros.                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(trace_level, 1).
-define(debug_level, 2).
-define(info_level, 3).
-define(error_level, 4).

-define(trace_str, "TRACE").
-define(debug_str, "DEBUG").
-define(info_str, "INFO").
-define(error_str, "ERROR").

-ifdef(log).
-define(TRACE(Format, Params), log:write(?trace_str, ?MODULE, ?LINE, Format, Params)).
-define(DEBUG(Format, Params), log:write(?debug_str, ?MODULE, ?LINE, Format, Params)).
-define(INFO(Format, Params), log:write(?info_str, ?MODULE, ?LINE, Format, Params)).
-define(ERROR(Format, Params), log:write(?error_str, ?MODULE, ?LINE, Format, Params)).
-define(TRACE(Format), log:write(?trace_str, ?MODULE, ?LINE, Format)).
-define(DEBUG(Format), log:write(?debug_str, ?MODULE, ?LINE, Format)).
-define(INFO(Format), log:write(?info_str, ?MODULE, ?LINE, Format)).
-define(ERROR(Format), log:write(?error_str, ?MODULE, ?LINE, Format)).
-else.
-define(TRACE(Format, Params), ok).
-define(DEBUG(Format, Params), ok).
-define(INFO(Format, Params), ok).
-define(ERROR(Format, Params), ok).
-define(TRACE(Format), ok).
-define(DEBUG(Format), ok).
-define(INFO(Format), ok).
-define(ERROR(Format), ok).
-endif.
