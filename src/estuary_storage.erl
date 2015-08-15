%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-module(estuary_storage).

-callback store(estuary_types:event(),
                estuary_types:schema(),
                estuary_types:container()) -> 'ok'|{'error', Reason :: string()}.
