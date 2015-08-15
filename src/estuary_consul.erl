%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_consul).

%% API
-export([fetch_schema/1]).

-include("estuary.hrl").

-define(DEFAULT_SCHEME, "http").
-define(DEFAULT_HOST,   "localhost").
-define(DEFAULT_PORT,   8500).
-define(PATH_TEMPLATE,  "/v1/kv/~s/~s.avsc").

fetch_schema(_Name) ->
  {ok, none}.




%% @spec build_path(list()) -> string()
%% @doc Build the path from a list of segments
%% @end
%%
build_path(Args) ->
  build_path(Args, []).


%% @spec build_path(string(), string()) -> string()
%% @doc Build the path from a list of segments
%% @end
%%
build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, percent_encode(Part)], "/"));
build_path([], Path) -> Path.


%% @spec get(Scheme, Host, Port, Path, Args) -> Result
%% @where Scheme = string(),
%%        Host   = string(),
%%        Port   = integer(),
%%        Path   = string(),
%%        Args   = proplist(),
%%        Result = {ok, mixed}|{error, Reason::string()}
%% @doc Perform a HTTP GET request
%% @end
%%
get(Scheme, Host, Port, Path, Args) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  lager:debug("GET ~s", [URL]),
  Response = httpc:request(URL),
  lager:debug("Response: [~p]", [Response]),
  parse_response(Response).


%% @spec build_uri(Scheme, Host, Port, Path, QArgs) -> string()
%% where Scheme = string()
%%       Host = string()
%%       Port = integer()
%%       Path = string()
%%       QArgs = proplist()
%% @doc Build the request URI
%% @end
%%
build_uri(Scheme, Host, Port, Path, QArgs) ->
  build_uri(string:join([Scheme, "://", Host, ":", estuary_util:as_string(Port)], ""), Path, QArgs).


%% @spec build_uri(string(), string(), proplist()) -> string()
%% @doc Build the requst URI for the given base URI, path and query args
%% @end
%%
build_uri(Base, Path, []) ->
  string:join([Base, build_path(Path)], "");
build_uri(Base, Path, QArgs) ->
  string:join([Base, string:join([build_path(Path),
                                  build_query(QArgs)], "?")], "").


%% @spec build_query(proplist()) -> string()
%% @doc Build the query parameters string from a proplist
%% @end
%%
build_query(Args) ->
  build_query(Args, []).


%% @spec build_query(proplist(), string()) -> string()
%% @doc Build the query parameters string from a proplist
%% @end
%%
build_query([{Key,Value}|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [string:join([percent_encode(Key),
                                                     percent_encode(Value)], "=")]));
build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([percent_encode(Key),
                                                     percent_encode(Value)], "=")]));
build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [percent_encode(Key)]));
build_query([], Parts) ->
  string:join(Parts, "&").


%% @spec decode_body(mixed) -> list()
%% @doc Decode the response body and return a list
%% @end
%%
decode_body(_, []) -> [];
decode_body(?JSON_MIME_TYPE, Body) ->
  case rabbit_misc:json_decode(estuary_util:as_string(Body)) of
    {ok, Value} -> Value;
    error       -> []
  end.


%% @spec parse_response(Response) -> {ok, string()} | {error, mixed}
%% @where Response = {status_line(), headers(), Body} | {status_code(), Body}
%% @doc Decode the response body and return a list
%% @end
%%
parse_response({error, Reason}) ->
  lager:error("HTTP Error ~p", [Reason]),
  {error, Reason};

parse_response({ok, 200, Body})  -> {ok, decode_body(?JSON_MIME_TYPE, Body)};
parse_response({ok, 201, Body})  -> {ok, decode_body(?JSON_MIME_TYPE, Body)};
parse_response({ok, 204, _})     -> {ok, []};
parse_response({ok, Code, Body}) ->
  lager:error("HTTP Response (~p) ~s", [Code, Body]),
  {error, integer_to_list(Code)};

parse_response({ok, {{_,200,_},Headers,Body}}) ->
  {ok, decode_body(proplists:get_value("content-type", Headers, ?JSON_MIME_TYPE), Body)};
parse_response({ok,{{_,201,_},Headers,Body}}) ->
  {ok, decode_body(proplists:get_value("content-type", Headers, ?JSON_MIME_TYPE), Body)};
parse_response({ok,{{_,204,_},_,_}}) -> {ok, []};
parse_response({ok,{{_Vsn,Code,_Reason},_,Body}}) ->
  lager:error("HTTP Response (~p) ~s", [Code, Body]),
  {error, integer_to_list(Code)}.


%% @spec percent_encode(Value) -> string()
%% @where
%%       Value = atom() or binary() or integer() or list()
%% @doc Percent encode the query value, automatically
%%      converting atoms, binaries, or integers
%% @end
%%
percent_encode(Value) ->
  http_uri:encode(estuary_util:as_string(Value)).
