%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_sup).

-behaviour(supervisor).

-export([start_link/1,
         init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Type, Config),
        {Id, {Module, start_link, Config}, permanent, 5, Type, [Module]}).


start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).


init([Config]) ->
    {ok, Vsn} = application:get_key(estuary, vsn),
    lager:info("Estuary v~s started", [Vsn]),

    C1 = [?CHILD(estuary_accumulator, estuary_accumulator, worker,
                 [proplists:get_value("storage", Config, [])]),
          ?CHILD(estuary_avro, estuary_avro, worker,
                 [proplists:get_value("avro_schema", Config, [])])],
    C2 = lists:append(C1, consumers(proplists:get_value("rabbitmq", Config, []))),
    C3 = lists:append(C2, filestorage(proplists:get_value("storage", Config, []))),
    C4 = lists:append(C3, s3storage(proplists:get_value("storage", Config, []))),
    {ok, {{one_for_one, 5, 10}, C4}}.


consumers(Config) ->
    Consumers = [list_to_atom(lists:flatten(io_lib:format("consumer~w", [N]))) ||
                    N <- lists:seq(1, proplists:get_value("qty", Config, 1))],
    [?CHILD(Consumer, estuary_consumer, worker,
            [{Consumer, Config}]) || Consumer <- Consumers].


filestorage(Config) ->
    case proplists:get_value("filesystem", Config, []) of
        [] -> [];
        FileConfig -> [?CHILD(estuary_storage_local, estuary_storage_local,
                              worker, [FileConfig])]
    end.


s3storage(Config) ->
    case proplists:get_value("s3", Config, []) of
        [] -> [];
        S3Config -> [?CHILD(estuary_storage_s3, estuary_storage_s3, worker, [S3Config])]
    end.
