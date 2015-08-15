%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_config).

%% API
-export([get/0]).

-include("estuary.hrl").

-include_lib("yamerl/include/yamerl_errors.hrl").

get() ->
  case find_configuration(?CONFIG_PATHS) of
    {ok, File} -> read_config(File);
    not_found  ->
      lager:error("Error: Could not find configuration"),
      {error, missing_configuration}
  end.

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

