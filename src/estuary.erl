%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary).

-behavior(application).

-export([start/0, start/2, stop/1]).

start() ->
  application:ensure_all_started(estuary),
  application:start(estuary).

start(_Type, _Args) ->
  case estuary_config:get() of
      {ok, Config} -> estuary_sup:start_link(Config);
      {error, Reason} -> {stop, Reason}
  end.

stop(_Reason) ->
  ok.
