%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_amqp).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("estuary.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(DEFAULT_HOST,      "localhost").
-define(DEFAULT_PORT,      5672).
-define(DEFAULT_VHOST,     "/").
-define(DEFAULT_USER,      "guest").
-define(DEFAULT_PASSWORD,  "guest").
-define(DEFAULT_HEARTBEAT, 60).
-define(DEFAULT_QUEUE,     "estuary").

-define(RECONNECT_DELAY,   5000).

-record(state, {config, connection, channel, queue, tag}).

start_link(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
  process_flag(trap_exit, true),
  {Connection, Channel} = connect_to_rabbitmq(amqp_config(Config)),
  Queue = list_to_binary(proplists:get_value("queue", Config)),
  start_consuming(Channel, Queue),
  {ok, #state{config=Config, connection=Connection, channel=Channel, queue=Queue}}.

handle_call(Request, _From, State) ->
  io:format("handle_call [~p]~n", [Request]),
  {reply, ok, State}.

handle_cast(reconnect, State) ->
  lager:info("Reconnecting"),
  {Connection, Channel} = connect_to_rabbitmq(amqp_config(State#state.config)),
  {ok, Tag} = start_consuming(Channel, list_to_binary(proplists:get_value("queue", State#state.config, ?DEFAULT_QUEUE))),
  {noreply, State#state{connection=Connection, channel=Channel, tag=Tag}};

handle_cast(Request, State) ->
  lager:debug("handle_cast [~p]", [Request]),
  {noreply, State}.

handle_info(#'basic.consume_ok'{consumer_tag=Tag}, State) ->
  lager:debug("Recieved Basic.ConsumeOk [~s]", [Tag]),
  {noreply, State};

handle_info({#'basic.deliver'{routing_key=Key, delivery_tag=Tag},
             #amqp_msg{props=Props, payload=Payload}}, State) ->
  wpool:call(estuary_aggregator, {process, Props#'P_basic'.content_type, Props#'P_basic'.type, Payload}, available_worker),
  amqp_channel:cast(State#state.channel, #'basic.ack'{delivery_tag = Tag}),
  {noreply, State};

handle_info({'EXIT', Pid, {shutdown,{server_initiated_close, Code, Text}}, _}, State=#state{channel=Chan}) when Pid == Chan ->
  lager:debug("Channel closed (~p) ~s", [Code, Text]),
  {ok, Chan} = amqp_connection:open_channel(State#state.connection),
  link(Chan),
  {ok, Tag} = start_consuming(Chan, State#state.config),
  {noreply, State#state{channel=Chan, tag=Tag}};

handle_info({'EXIT', Pid, {shutdown,{server_initiated_close, Code, Text}}, _}, State=#state{connection=Conn}) when Pid == Conn ->
  lager:info("Connection closed (~p) ~s", [Code, Text]),
  timer:apply_after(?RECONNECT_DELAY, gen_server, cast, [?MODULE, reconnect]),
  {noreply, State#state{connection=none, channel=none, tag=none}};

handle_info({'EXIT', Pid, socket_closed_unexpectedly}, State=#state{connection=Conn}) when Pid == Conn ->
  lager:info("Connection reset ~p", [Conn]),
  timer:apply_after(?RECONNECT_DELAY, gen_server, cast, [?MODULE, reconnect]),
  {noreply, State#state{connection=none, channel=none, tag=none}};

handle_info({'EXIT', Pid, Reason}, State) ->
  lager:debug("Exit: ~p ~p", [Pid, Reason]),
  {noreply, State};

handle_info(Info, State) ->
  lager:debug("Received info: ~p", [Info]),
  {noreply, State}.

terminate(Reason, State) ->
  lager:info("Terminate: ~p", [Reason]),
  amqp_channel:call(State#state.channel, #'basic.cancel'{consumer_tag=State#state.tag}),
  amqp_channel:close(State#state.channel),
  amqp_connection:close(State#state.connection),
  lager:info("AMQP publisher stopped"),
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
               heartbeat=proplists:get_value("heartbeat", Config, ?DEFAULT_HEARTBEAT)}.

%% @spec connect_to_rabbitmq(#amqp_config{}) -> {pid(), pid()}
%% @doc Connect to RabbitMQ and open a channel
%% @end
%%
connect_to_rabbitmq(Config) ->
  case amqp_connection:start(#amqp_params_network{host=Config#amqp_config.host,
                                                  port=Config#amqp_config.port,
                                                  virtual_host=Config#amqp_config.vhost,
                                                  username=Config#amqp_config.user,
                                                  password=Config#amqp_config.password,
                                                  heartbeat=Config#amqp_config.heartbeat}) of
    {ok, Conn} ->
      {ok, Chan} = amqp_connection:open_channel(Conn),
      lager:info("Connected to RabbitMQ"),
      link(Conn),
      link(Chan),
      {Conn, Chan};
    {error, Error} ->
      lager:info("Error connecting to RabbitMQ: ~p", [Error]),
      {null, null}
  end.

%% @spec start_consuming(pid(), binary()) -> {ok, binary()}
%% @doc Send a Basic.Consume RPC request for the configured estuary queue
%% @end
%%
start_consuming(null, null) -> {error, no_connection};
start_consuming(Channel, Queue) ->
  #'basic.consume_ok'{consumer_tag=Tag} = amqp_channel:call(Channel, #'basic.consume'{queue=Queue}),
  lager:debug("Registered, Consumer ~s", [Tag]),
  {ok, Tag}.
