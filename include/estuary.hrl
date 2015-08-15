%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================

-define(CONFIG_PATHS, ["estuary.yaml", "/etc/estuary.yaml", "/etc/estuary/estuary.yaml"]).

-define(DATUM_MIME_TYPE, <<"application/vnd.apache.avro.datum">>).
-define(JSON_MIME_TYPE, "application/json").

-record(amqp_config, {host, port, vhost, user, password, heartbeat, prefetch_count}).
