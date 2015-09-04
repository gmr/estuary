-module(estuary_path_tests).

-include_lib("eunit/include/eunit.hrl").

all_fields_test() ->
  Datetime = {{2002,12,20},{7,46,0}},
  Path = "/%Y/%y/%m/%d/%H/%{type}/%{host}",
  {ok, Hostname} = inet:gethostname(),
  Expectation = {ok, "/2002/02/12/20/07/test/" ++ Hostname},
  ?assertEqual(Expectation, estuary_path:format(Path, "test", Datetime)).
