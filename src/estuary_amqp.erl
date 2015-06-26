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

-record(state, {config, connection, channel, tag}).

start_link(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
  RabbitConfig = #amqp_config{host=proplists:get_value("host", Config),
                              port=proplists:get_value("port", Config),
                              vhost=list_to_binary(proplists:get_value("vhost", Config)),
                              user=list_to_binary(proplists:get_value("user", Config)),
                              password=list_to_binary(proplists:get_value("password", Config))},

  {Connection, Channel} = connect_to_rabbitmq(RabbitConfig),

  {ok, Tag} = start_consuming(Channel, list_to_binary(proplists:get_value("queue", Config))),

  process_flag(trap_exit, true),

  {ok, #state{config=RabbitConfig, connection=Connection, channel=Channel, tag=Tag}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'basic.consume_ok', Tag}, State) ->
  io:format("Recieved Basic.ConsumeOk [~s]~n", [Tag]),
  {noreply, State};

handle_info({#'basic.deliver'{routing_key=Key, delivery_tag=Tag}, #amqp_msg{props=Props, payload=Payload}}, State) ->
  io:format("Received message [~s] [~p]~n[~p]~n", [Key, Props, Payload]),
  amqp_channel:cast(State#state.channel, #'basic.ack'{delivery_tag = Tag}),
  {noreply, State}.

terminate(_Reason, State) ->
  amqp_channel:call(State#state.channel, #'basic.cancel'{consumer_tag=State#state.tag}),
  amqp_channel:close(State#state.channel),
  amqp_connection:close(State#state.connection),
  io:format("AMQP publisher stopped~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


connect_to_rabbitmq(Config) ->
  case amqp_connection:start(#amqp_params_network{host=Config#amqp_config.host,
                                                  port=Config#amqp_config.port,
                                                  virtual_host=Config#amqp_config.vhost,
                                                  username=Config#amqp_config.user,
                                                  password=Config#amqp_config.password}) of
    {ok, Conn} ->
      {ok, Chan} = amqp_connection:open_channel(Conn),
      io:format("Connected to RabbitMQ~n"),
      {Conn, Chan};
    {error, Error} ->
      io:format("Error connecting to RabbitMQ: ~p~n", [Error]),
      {null, null}
  end.

start_consuming(null, null) -> {error, no_connection};
start_consuming(Channel, Queue) ->
  #'basic.consume_ok'{consumer_tag=Tag} = amqp_channel:call(Channel, #'basic.consume'{queue=Queue}),
  io:format("Registered, Consumer ~s~n", [Tag]),
  {ok, Tag}.
