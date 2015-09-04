%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_path).

-export([format/2, format/3]).

-define(CONVERSION_SPECIFICATIONS, ["%{host}", "%{type}"]).

%% @spec format(Path, Type) -> string()
%% @where
%%    Path = string()
%%    Type = string()
%% @doc Return a interpolated version of the path string. Use the current UTC
%%      datetime for the data/time interpolation elements.
%% @end
%%
format(Path, Type) ->
    format(Path, Type, calendar:universal_time()).


%% @spec format(Path, Type, Datetime) -> string()
%% @where
%%    Path = string()
%%    Type = string()
%%    Datetime = datetime()
%% @doc Return a interpolated version of the path string.
%% @end
%%
format(Path, Type, Datetime) ->
    Path1 = strftimerl:format(Path, Datetime),
    {ok, replace(?CONVERSION_SPECIFICATIONS, Type, Datetime, Path1)}.


%% @private
%% @spec hostname(mixed) -> string()
%% @doc Return the machine's hostname
%% @end
%%
hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.


%% @private
%% @spec replace([string()], string(), calendar:universal_time(), string()) -> string()
%% @doc Replace format specifiers with the correct values
%% @end
%%
replace([], _, _, Path) -> Path;

replace([H|T], Type, Datetime, Path) when H == "%{host}" ->
    Value = re:replace(Path, "%\{host\}", hostname(), [global, {return, list}]),
    replace(T, Type, Datetime, Value);

replace([H|T], Type, Datetime, Path) when H == "%{type}" ->
    Value = re:replace(Path, "%\{type\}", estuary_util:as_string(Type), [global, {return, list}]),
    replace(T, Type, Datetime, Value);

replace([H|T], Type, Datetime, Path) ->
    lager:warning("Unsupported specifier ~s", [H]),
    replace(T, Type, Datetime, Path).
