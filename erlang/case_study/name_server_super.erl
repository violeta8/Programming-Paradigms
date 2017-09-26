%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Extends the generic supervisor and implements the behavior of a name server
%%% supervisor that can restart a name server process when it crashes.
%%% ----------------------------------------------------------------------------
-module(name_server_super).

%%% Name server supervisor exports.
-export([start/1]).

%%% Callback exports.
-export([init/1]).

%%% ------------------------------------------------------------------------ %%%
%%% Supervisor API.                                                          %%%
%%% ------------------------------------------------------------------------ %%%


%% -----------------------------------------------------------------------------
%% Starts the supervisor with the specified restart strategy.
%% Two possible strategies are available:
%% 1. 'always': this means that the child process can be restarted by the
%%    supervisor an infinite number of times when it crashes.
%% 2. '{max, Max}': this instructs the supervisor to restart a crashed child
%%    process a fixed number of times, as specified by 'Max'. When 'Max' is
%%    reached, the child process is not restarted and the supervisor itself
%%    crashes.
%% -----------------------------------------------------------------------------
start(Strategy) ->
  super_gen:start(?MODULE, Strategy).


%%% ------------------------------------------------------------------------ %%%
%%% Callbacks.                                                               %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Handles supervisor initialisation.
%%
%% Note: the init/1 function is called *automatically* by super_gen when it
%% starts, to allow (actual call is done by super_gen:init/2, but that is
%% invisible from us and we should not care) us to perform *custom*
%% initialisation. The returned '{Strategy, Mfa}' is then used by super_gen to
%% start its own process loop.
%% -----------------------------------------------------------------------------
init(Args) ->
  {Args, {name_server, start_link, []}}.
