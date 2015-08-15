-module(estuary_util_tests).

-include_lib("eunit/include/eunit.hrl").

as_atom_atom_test() ->
  ?assertEqual(foo, estuary_util:as_atom(foo)).

as_atom_binary_test() ->
  ?assertEqual(bar, estuary_util:as_atom(<<"bar">>)).

as_atom_list_test() ->
  ?assertEqual(baz, estuary_util:as_atom("baz")).

as_integer_list_test() ->
  ?assertEqual(42, estuary_util:as_integer("42")).

as_integer_integer_test() ->
  ?assertEqual(42, estuary_util:as_integer(42)).

as_string_binary_test() ->
  ?assertEqual("foo", estuary_util:as_string(<<"foo">>)).

as_string_list_test() ->
  ?assertEqual("bar", estuary_util:as_string("bar")).

as_string_integer_test() ->
  ?assertEqual("42", estuary_util:as_string(42)).
