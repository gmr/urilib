%% =============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016
%% @doc urilib is a RFC-3986 URI Library for Erlang
%% @end
%% =============================================================================

-record(authority, {host :: string(),
                    port :: integer()}).

-record(userinfo, {username :: string(),
                   password :: string()}).

-record(uri, {scheme :: atom(),
              userinfo :: #userinfo{},
              authority :: #authority{},
              path :: string(),
              query :: list(),
              fragment :: string()}).

-record(url, {scheme :: atom(),
              username :: string(),
              password :: string(),
              host :: string(),
              port :: integer(),
              path :: string(),
              query :: list(),
              fragment :: string()}).
