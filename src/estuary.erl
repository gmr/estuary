%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary).

-export([main/1]).

-include_lib("yamerl/include/yamerl_errors.hrl").

%% Command line argument specification
-define(SPEC,
        [{config,     $c, "config",     list,    "Config file"},
         {initialize, $i, "initialize", boolean, "initialize missing resources"},
         {help,       $h, "help",       boolean, "display this help and exit"}]).

%% @spec main(Args)
%% @where
%%       Args = list()
%% @doc Process the command line arguments, displaying help or running app
%% @end
%%
main(Args) ->
  case getopt:parse(?SPEC, Args) of
    {ok, {Opts, _}} ->
      case proplists:get_bool(help, Opts) of
        true  -> usage(0);
        false ->
          case process(Opts) of
            {ok, Config} ->
              start_applications(),
              {ok, _Pid} = estuary_sup:start_link(Config),
              receive
                {'EXIT', _From, _Reason} ->
                  io:format('Caught Exit')
              end;
            {error, Error} ->
              io:format("Error: ~s~n~n", [Error]),
              usage(1)
          end
      end;
    {error, _} -> usage(1)
  end.

%% @private
%% @spec usage(ExitCode)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
usage(ExitCode) ->
  getopt:usage(?SPEC, escript:script_name()),
  erlang:halt(ExitCode).


%% @private
%% @spec process(Opts)
%% @where
%%       ExitCode = integer()
%% @doc Display the cli help, exiting with the specified exit code
%% @end
%%
process(Opts) ->
  case proplists:get_value(config, Opts, error) of
    error -> {error, "Configuration file not specified"};
    ConfigFile ->
      case filelib:is_file(ConfigFile) of
        true -> read_config(ConfigFile);
        false ->
          io:format("Error: Could not read ~s~n", [ConfigFile]),
          erlang:halt(1)
      end
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


%% @private
%% @spec start_applications()
%% @doc We could use ensure_all_started(estuary) but since there isn't an
%%     application behavior, this workers better.
%% @end
%%
start_applications() ->
  application:start(kernel),
  application:start(stdlib),
  application:start(crypto),
  application:start(ssl),
  application:start(xmerl),
  application:start(rabbit_common),
  application:start(amqp_client),
  application:start(eavro),
  application:start(erlcloud),
  application:start(folsom),
  application:start(lager).
