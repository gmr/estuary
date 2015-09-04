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

-define(DEFAULT_TYPE,   "file").
-define(DEFAULT_PATH,   "schemas").

-record(state, {type, path, template, schemas}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
    case proplists:get_value("type", Config, ?DEFAULT_TYPE) of
        "file"   ->
            {ok, #state{type=file,
                        path=proplists:get_value("path", Config, ?DEFAULT_PATH),
                        schemas=[]}};
        "consul" ->
            %%Template = url_template(Config),
            {ok, #state{type=consul, schemas=[]}}
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
