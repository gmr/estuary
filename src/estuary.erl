%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary).

-behavior(application).

-export([start/0, start/2, stop/1]).

-define(PATHS, ["estuary.yaml", "/etc/estuary.yaml"]).

-include_lib("yamerl/include/yamerl_errors.hrl").


start() ->
  application:ensure_all_started(estuary),
  application:start(estuary).

%% @spec main(Args)
%% @where
%%       Args = list()
%% @doc Process the command line arguments, displaying help or running app
%% @end
%%
start(_Type, _Args) ->
  lager:debug("cwd: ~p", [file:get_cwd()]),
  case find_configuration(?PATHS) of
    {ok, File} ->
      {ok, Config} = read_config(File),
      estuary_sup:start_link(Config);
    not_found ->
      lager:error("Error: Could not find configuration"),
      {stop, missing_configuration}
  end.


stop(_Reason) ->
  ok.

find_configuration([]) -> not_found;
find_configuration([File|Files]) ->
  lager:debug("Checking if ~p is a file: ~p", [File, filelib:is_file(File)]),
  case filelib:is_file(File) of
    true -> {ok, File};
    false -> find_configuration(Files)
  end.

%% @private
%% @spec read_config(ConfigFile)
%% @where
%%       ConfigFile = list()
%% @doc Read the configuration file from disk and return the config values
%% @end
%%
read_config(ConfigFile) ->
  application:start(yamerl),
  case file:read_file(ConfigFile) of
    {ok, Data} -> {ok, yamerl_constr:string(Data)};
    {error, Error} -> {error, Error}
  end.
