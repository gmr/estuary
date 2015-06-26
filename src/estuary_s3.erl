-module(estuary_s3).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-record(state, {config, s3bucket}).

start_link(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
  AWSConfig = #aws_config{access_key_id=proplists:get_value("access_key_id", Config),
                          secret_access_key=proplists:get_value("secret_access_key", Config)},
  {ok, #state{config=AWSConfig, s3bucket=proplists:get_value("s3bucket", Config)}}.

handle_call(list_buckets, _From, State) ->
  {reply, list_buckets(), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

list_buckets() ->
  erlcloud_s3:list_buckets().
