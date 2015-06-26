%%=============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%=============================================================================
-module(estuary).

-export([main/1]).

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
            ok    -> ok;
            Error ->
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
  getopt:usage(?SPEC, "estuary", [], []),
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
    error -> "Configuration file not specified";
    ConfigFile ->
      case filelib:is_file(ConfigFile) of
        true ->
          read_config(ConfigFile),
          ok;
        false ->
          io:format("Error: Could not read ~s~n", [ConfigFile]),
          erlang:halt(1)
      end
  end.


%% @private
%% @spec read_config(ConfigFile)
%% @where
%%       ExitCode = integer()
%% @doc Read the configuration file from disk and return the config values
%% @end
%%
read_config(ConfigFile) ->
  Config = yamerl_constr:file(ConfigFile),
  io:format("Config: ~p~n", [Config]),
  ok.
