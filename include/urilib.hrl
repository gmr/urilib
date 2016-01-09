%% =============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016
%% @end
%% =============================================================================

-record(authority, {host :: string(),
                    port :: integer()}).

-record(userinfo, {username :: string() | undefined,
                   password :: string() | undefined}).

-record(uri, {scheme :: atom(),
              userinfo :: #userinfo{} | undefined,
              authority :: #authority{},
              path :: string() | undefined,
              query,
              fragment :: string() | undefined}).

-record(url, {scheme :: atom(),
              username :: string() | undefined,
              password :: string() | undefined,
              host :: string(),
              port :: integer(),
              path :: string() | undefined,
              query,
              fragment :: string() | undefined}).
