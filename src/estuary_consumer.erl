%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_consumer).

-behaviour(amqp_gen_consumer).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("estuary.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_HOST,           "localhost").
-define(DEFAULT_PORT,           5672).
-define(DEFAULT_VHOST,          "/").
-define(DEFAULT_USER,           "guest").
-define(DEFAULT_PASSWORD,       "guest").
-define(DEFAULT_QUEUE,          "estuary").
-define(DEFAULT_HEARTBEAT,      60).
-define(DEFAULT_PREFETCH_COUNT, 1).

-define(RECONNECT_DELAY,        5000).

-record(state, {id, config, connection, channel, queue, tag, prefetch_count, deliveries}).

start_link({Id, Config}) ->
    gen_server:start_link({local, Id}, ?MODULE, {Id, Config}, []).

init({Id, Config}) ->
    lager:info("~s starting", [Id]),
    process_flag(trap_exit, true),

    %% Initialize new counters
    [folsom_metrics:new_counter(C) || C <- [consumer_channels_closed, consumer_messages_received,
                                            consumer_unhandled_casts, consumer_unhandled_calls,
                                            consumer_unhandled_infos]],

    %% Connect to RabbitMQ and start consuming messages
    {Connection, Channel} = connect_to_rabbitmq(Id, amqp_config(Config)),
    Queue = list_to_binary(proplists:get_value("queue", Config)),
    start_consuming(Id, Channel, Queue),

    {ok, #state{id=Id, config=Config, connection=Connection, channel=Channel, queue=Queue, deliveries=0,
                prefetch_count=proplists:get_value("prefetch_count", Config, ?DEFAULT_PREFETCH_COUNT)}}.

handle_call(Request, _From, State) ->
    lager:warning("~s unhandled call: ~p", [State#state.id, Request]),
    folsom_metrics:notify({consumer_unahndled_calls, {inc, 1}}),
    {reply, ok, State}.

handle_cast(reconnect, State) ->
    lager:info("~s reconnecting", [State#state.id]),
    {Connection, Channel} = connect_to_rabbitmq(State#state.id, amqp_config(State#state.config)),
    {ok, Tag} = start_consuming(State#state.id, Channel,
                                list_to_binary(proplists:get_value("queue", State#state.config, ?DEFAULT_QUEUE))),
    {noreply, State#state{connection=Connection, channel=Channel, tag=Tag}};

handle_cast(Request, State) ->
    lager:warning("~s unhandled cast: ~p", [State#state.id, Request]),
    folsom_metrics:notify({consumer_unahndled_casts, {inc, 1}}),
    {noreply, State}.

handle_info(#'basic.consume_ok'{consumer_tag=Tag}, State) ->
    lager:debug("~s recieved Basic.ConsumeOk [~s]", [State#state.id, Tag]),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag=DeliveryTag},
             #amqp_msg{props=Props, payload=Payload}}, State) ->
    wpool:call(estuary_accumulator, {process,
                                     Props#'P_basic'.content_type,
                                     Props#'P_basic'.type, Payload}, available_worker),
    folsom_metrics:notify({messages_received, {inc, 1}}),
    Deliveries = State#state.deliveries + 1,

    case State#state.prefetch_count of
      Deliveries ->
          lager:info("~s multi-acking delivery tag ~p", [State#state.id, DeliveryTag]),
          amqp_channel:cast(State#state.channel, #'basic.ack'{delivery_tag = DeliveryTag, multiple = true}),
          {noreply, State#state{deliveries=0}};
        _ ->
          {noreply, State#state{deliveries=Deliveries}}
    end;

handle_info({'EXIT', Pid, {shutdown,{server_initiated_close, Code, Text}}, _}, State=#state{channel=Chan}) when Pid == Chan ->
    lager:debug("~s channel closed (~p) ~s", [State#state.id, Code, Text]),
    folsom_metrics:notify({consumer_channels_closed, {inc, 1}}),
    Chan = open_channel(State#state.connection, State#state.config),
    {ok, Tag} = start_consuming(State#state.id, Chan, State#state.config),
    {noreply, State#state{channel=Chan, tag=Tag}};

handle_info({'EXIT', _Pid, {shutdown,{app_initiated_close, Code, Text}}, _}, State) ->
    lager:info("~s connection closed (~p) ~s", [State#state.id, Code, Text]),
    {noreply, State#state{connection=null, channel=null}};

handle_info({'EXIT', _Pid, {shutdown,{connection_closing,{server_initiated_close, Code, Text}}}}, State) ->
    lager:debug("~s received connection closing notification (~p) ~s", [State#state.id, Code, Text]),
    {noreply, State};

handle_info({'EXIT', _Pid, {shutdown,{server_initiated_close, Code, Text}}}, State) ->
    remote_close(Code, Text, State);

handle_info({'EXIT', _Pid, {shutdown,{server_misbehaved, Code, Text}}}, State) ->
    remote_close(Code, Text, State);

handle_info({'EXIT', _Pid, {shutdown,{internal_error, Code, Text}}}, State) ->
    remote_close(Code, Text, State);

handle_info({'EXIT', Pid, Reason}, State) ->
    lager:warning("~s unhandled exit: ~p (~p) ~p", [State#state.id, Pid, State#state.connection, Reason]),
    {noreply, State#state{connection=null, channel=null}};

handle_info(Info, State) ->
    lager:info("~s handle_info: ~p", [State#state.id, Info]),
    folsom_metrics:notify({consumer_unahndled_infos, {inc, 1}}),
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        {shutdown, Term} ->
            lager:info("~s terminating: ~s", [State#state.id, Term]);
        _ ->
            lager:info("~s terminating: ~p", [State#state.id, Reason])
    end,
    case State#state.connection of
        null ->
            lager:debug("~s terminating while not connected", [State#state.id]);
        _ ->
            lager:debug("~s cancelling the consumer and gracefully closing", [State#state.id]),
            amqp_channel:call(State#state.channel, #'basic.cancel'{consumer_tag=State#state.tag}),
            amqp_channel:close(State#state.channel),
            amqp_connection:close(State#state.connection)
    end,
    lager:info("~s shutdown complete", [State#state.id]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @spec amqp_config(proplist()) -> #amqp_config{}
%% @doc Return the AMQP config record for connecting to RabbitMQ
%% @end
%%
amqp_config(Config) ->
    #amqp_config{host=proplists:get_value("host", Config, ?DEFAULT_HOST),
                 port=proplists:get_value("port", Config, ?DEFAULT_PORT),
                 vhost=list_to_binary(proplists:get_value("vhost", Config, ?DEFAULT_VHOST)),
                 user=list_to_binary(proplists:get_value("user", Config, ?DEFAULT_USER)),
                 password=list_to_binary(proplists:get_value("password", Config, ?DEFAULT_PASSWORD)),
                 heartbeat=proplists:get_value("heartbeat", Config, ?DEFAULT_HEARTBEAT),
                 prefetch_count=proplists:get_value("prefetch_count", Config, ?DEFAULT_PREFETCH_COUNT)}.

%% @spec connect_to_rabbitmq(#amqp_config{}) -> {pid(), pid()}
%% @doc Connect to RabbitMQ and open a channel
%% @end
%%
connect_to_rabbitmq(Id, Config) ->
    case amqp_connection:start(#amqp_params_network{host=Config#amqp_config.host,
                                                    port=Config#amqp_config.port,
                                                    virtual_host=Config#amqp_config.vhost,
                                                    username=Config#amqp_config.user,
                                                    password=Config#amqp_config.password,
                                                    heartbeat=Config#amqp_config.heartbeat}) of
        {ok, Conn} ->
            lager:info("~s connected to RabbitMQ", [Id]),
            link(Conn),
            Chan = open_channel(Conn, Config),
            {Conn, Chan};
        {error, Error} ->
            lager:error("~s could not connect to RabbitMQ: ~p", [Id, Error]),
            {null, null}
    end.

%% @spec open_channel(pid()) -> pid()
%% @doc Open a channel and set the prefetch count
%% @end
%%
open_channel(Conn, Config) ->
    {ok, Chan} = amqp_connection:open_channel(Conn),
    link(Chan),
    amqp_channel:call(Chan, #'basic.qos'{prefetch_count=Config#amqp_config.prefetch_count}),
    Chan.


%% @spec remote_close(non_neg_integer(), binary(), #state{}) -> {stop, {shutdown, binary()}, #state{}}
%% @doc Log the remote close message and return the response for shutting down the consumer
%% @end
%%
remote_close(Code, Text, State) ->
    lager:warning("~s connection closed (~p) ~s", [State#state.id, State#state.connection, Code, Text]),
    {stop, {shutdown, Text}, State#state{connection=null, channel=null}}.


%% @spec start_consuming(pid(), binary()) -> {ok, binary()}
%% @doc Send a Basic.Consume RPC request for the configured estuary queue
%% @end
%%
start_consuming(_, null, null) -> {error, no_connection};
start_consuming(Id, Channel, Queue) ->
    #'basic.consume_ok'{consumer_tag=Tag} = amqp_channel:call(Channel, #'basic.consume'{queue=Queue}),
    lager:debug("~s registered, Consumer ~s", [Id, Tag]),
    {ok, Tag}.
