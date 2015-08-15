%% =============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%% =============================================================================
-record(amqp_config, {host, port, vhost, user, password, heartbeat, prefetch_count}).
