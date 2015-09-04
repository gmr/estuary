%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(estuary_util).

%% API
-export([as_atom/1,
         as_integer/1,
         as_string/1]).


%% @spec as_atom(Value) -> list()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
as_atom(Value) when is_atom(Value) =:= true -> Value;
as_atom(Value) when is_binary(Value) =:= true -> list_to_atom(binary_to_list(Value));
as_atom(Value) when is_list(Value) =:= true -> list_to_atom(Value);
as_atom(Value) ->
  autocluster_log:error("Unexpected data type for atom value: ~p~n", [Value]),
  Value.


%% @spec maybe_convert_to_int(Value) -> integer()
%% where Value = list()|integer()
%% @doc Return the value as an integer
%% @end
%%
as_integer([]) -> undefined;
as_integer(Value) when is_list(Value) =:= true -> list_to_integer(Value);
as_integer(Value) when is_integer(Value) =:= true -> Value;
as_integer(Value) ->
  autocluster_log:error("Unexpected data type for integer value: ~p~n", [Value]),
  Value.


%% @spec as_string(Value) -> list()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
as_string([]) -> undefined;
as_string(Value) when is_atom(Value) =:= true -> atom_to_list(Value);
as_string(Value) when is_binary(Value) =:= true -> binary_to_list(Value);
as_string(Value) when is_integer(Value) =:= true -> integer_to_list(Value);
as_string(Value) when is_list(Value) =:= true -> Value;
as_string(Value) ->
  autocluster_log:error("Unexpected data type for list value: ~p~n", [Value]),
  Value.
