%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_avro).

-behavior(gen_server).

-export([start_link/1,
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("deps/eavro/include/eavro.hrl").

-define(REQUEST_TIMEOUT, 5000).  %% 5 seconds

-define(DEFAULT_TYPE,   "file").
-define(DEFAULT_PATH,   "schemas").
-define(DEFAULT_SCHEME, "http").
-define(DEFAULT_HOST,   "localhost").
-define(DEFAULT_PORT,   8500).
-define(TEMPLATE_PATH,  "localhost").
-define(PATH_TEMPLATE,  "/v1/kv/schema/avro/~s/~s.avsc").

-record(state, {type, path, template, schemas}).

start_link(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
  case proplists:get_value("type", Config, ?DEFAULT_TYPE) of
    "file"   ->
      {ok, #state{type=file, path=proplists:get_value("path", Config, ?DEFAULT_PATH), schemas=[]}};
    "consul" ->
      Template = url_template(Config),
      {ok, #state{type=consul, template=Template, schemas=[]}}
  end.

terminate(_, _) ->
  ok.

code_change(_, _, State) ->
  {ok, State}.

handle_call({get, Schema}, _, State) ->
  {reply, proplists:get_value(Schema, State#state.schemas), State};

handle_call(_, _, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

get_schema(_Schema, #state{type=Type}) when Type == file ->
  {error, not_implemented};

get_schema(_Schema, #state{type=Type, template=Template}) when Type == consul ->
  URL = io_lib:format(Template, []),
  lager:info("Fetching schema at ~s", [URL]),
  case httpc:request(get, {URL, [{"Accept", "application/json"}]},
                     [{timeout, ?REQUEST_TIMEOUT}], []) of
    {ok, {{_Version, 200, _Message}, _Headers, Body}} ->
      {ok, eavro:parse_schema(list_to_binary(Body))};
    {error, Reason} ->
      lager:error("Error fetching Avro Schema: ~p", [Reason]),
      {error, Reason}
  end.


url_template(Config) ->
  Scheme = proplists:get_value("scheme", Config, ?DEFAULT_SCHEME),
  Host = proplists:get_value("host", Config, ?DEFAULT_HOST),
  Port = proplists:get_value("port", Config, ?DEFAULT_PORT),
  string:join([Scheme, "://", Host, ":", integer_to_list(Port), ?PATH_TEMPLATE, "?raw"], "").

