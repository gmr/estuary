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
-define(CHILD(I, Type, Config), {I, {I, start_link, Config}, permanent, 5, Type, [I]}).

%% =============================================================================
%% API functions
%% =============================================================================

start_link(Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

%% ?CHILD(estuary_s3, worker, [proplists:get_value("aws", Config)])

init([Config]) ->
  {ok, Vsn} = application:get_key(estuary, vsn),
  lager:info("Estuary v~s started", [Vsn]),
  {ok, {{one_for_one, 5, 10},
        [?CHILD(estuary_aggregator, worker, []),
         ?CHILD(estuary_avro, worker, [proplists:get_value("avro", Config, [])]),
         ?CHILD(estuary_s3, worker, [proplists:get_value("aws", Config, [])]),
         ?CHILD(estuary_amqp, worker, [proplists:get_value("rabbitmq", Config, [])])]}}.
