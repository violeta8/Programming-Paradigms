%%%-------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%% Created: 20 Sep 2017
%%%-------------------------------------------------------------------
-module(name_server_super).
-export([start/1]). % Server exports.
-export([init/1]). % Callback exports.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Supervisor functions.                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Starts the supervisor with the specified restart strategy.
start(Strategy) ->
  super_gen:start(?MODULE, Strategy).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback functions.                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Handles supervisor initialisation.
%
% Note: the init/1 function is called *automatically* by super_gen when it
% starts, to allow (actual call is done by super_gen:init/2, but that is
% invisible from us and we should not care) us to perform *custom*
% initialisation. The returned '{Strategy, Mfa}' is then used by super_gen to
% start its own process loop.
init(Args) ->
  {Args, {name_server, start_link, []}}.
