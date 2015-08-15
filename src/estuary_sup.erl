%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Type, Config), {Id, {Module, start_link, Config}, permanent, 5, Type, [Module]}).


%% =============================================================================
%% API functions
%% =============================================================================

start_link(Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

init([Config]) ->
  {ok, Vsn} = application:get_key(estuary, vsn),
  lager:info("Estuary v~s started", [Vsn]),

  C1 = [?CHILD(estuary_accumulator, estuary_accumulator, worker, []),
        ?CHILD(estuary_avro, estuary_avro, worker, [proplists:get_value("avro", Config, [])]),
        ?CHILD(estuary_s3, estuary_s3, worker, [proplists:get_value("aws", Config, [])])],
  C2 = consumers(proplists:get_value("rabbitmq", Config, [])),
  Children = lists:append(C1, C2),
  {ok, {{one_for_one, 5, 10}, Children}}.

consumers(Config) ->
  Consumers = [list_to_atom(lists:flatten(io_lib:format("consumer~w", [N]))) ||
               N <- lists:seq(1, proplists:get_value("qty", Config, 1))],
  [?CHILD(Consumer, estuary_consumer, worker, [{Consumer, Config}]) || Consumer <- Consumers].
