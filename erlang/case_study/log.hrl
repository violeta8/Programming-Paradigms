%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a number of macros that act as the frontend to the logging
%%% functions in log.hrl.
%%% ----------------------------------------------------------------------------


%%% ------------------------------------------------------------------------ %%%
%%% Client configuration macros.                                             %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Comment the line below to disable logging entirely (i.e. logging statements
%% in the code do not show up).
%% -----------------------------------------------------------------------------
-define(log, log).

%% -----------------------------------------------------------------------------
%% The macro log_level controls the logging level. It can be set to a value
%% from 1 to 4 (inclusive). Log statements are output if the value of
%% log_level is greater or equal to the logging level of the log statement being
%% output.
%% There are 4 logging levels:
%% 1 = TRACE
%% 2 = DEBUG
%% 3 = INFO
%% 4 = ERROR
%%
%% Example:
%%   If the log statement being output is DEBUG (i.e. level 2), and the current
%%   logging level is that of TRACE (i.e. log_level = 1), then the log is output
%%    to the screen because the level for DEBUG is greater than that of TRACE.
%% Example:
%%   If the log statement being output is INFO (i.e. level 3), but the current
%%   logging level is that of ERROR (i.e. log_level = 4), then the log is not
%%   output to the screen because the level for INFO is less than that of
%%   ERROR. To be able to see the log, the log level must be set to any one of
%%   TRACE (i.e. log_level = 1), DEBUG (i.e. log_level = 2) or INFO
%%   (i.e. log_level = 3).
%% -----------------------------------------------------------------------------
-ifndef(log_level).
-define(log_level, 1).
-endif.


%%% ------------------------------------------------------------------------ %%%
%%% Internal macros (DO NOT MODIFY).                                         %%%
%%% ------------------------------------------------------------------------ %%%

%%% Trace level definitions (DO NOT CHANGE).
-define(trace_level, 1).
-define(debug_level, 2).
-define(info_level, 3).
-define(error_level, 4).

%%% Trace level string definitions (DO NOT CHANGE).
-define(trace_str, "TRACE").
-define(debug_str, "DEBUG").
-define(info_str, "INFO").
-define(error_str, "ERROR").

%%% ------------------------------------------------------------------------ %%%
%%% Client API macros.                                                       %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Provide a simple and clean logging interface that can be used to output
%% logging statements on screen.
%%
%% The available macros
%% Example usage:
%%   ?INFO("Hello my name is ~p ~p.", [duncan, attard]) where:
%%   the format string includes two parameters, shown by ~p, whose values are
%%   given in the list [duncan, attard] (works like the standard io:format/1-3
%%   functions).
%% The short form when no parameters are required is:
%%   ?INFO("Hello world!").
%% -----------------------------------------------------------------------------
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
