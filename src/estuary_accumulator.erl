%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_accumulator).

-behavior(gen_server).

-export([start_link/1,
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include("estuary.hrl").

-record(state, {children, config}).

start_link(Config) ->
    wpool:start_pool(?MODULE, [{workers, 10},
                               {overrun_warning, 120000},
                               {worker, {?MODULE, Config}}]).

init(Config) ->
    {ok, #state{children=[], config=Config}}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call({process, ContentType, Type, _Payload}, _From, State) ->
    case binary_to_list(ContentType) of
        ?DATUM_MIME_TYPE ->
            lager:debug("Type: ~s", [Type]),
            {reply, ok, State};
        Other ->
            lager:error("Unsupported content type: ~s", [Other]),
            {reply, error, State}
    end;

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.
