%% =============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016
%% @doc urilib is a RFC-3986 URI Library for Erlang
%% @end
%% =============================================================================
-module(urilib).

-export([build/1,
         parse_uri/1,
         parse_url/1,
         encode/1,
         encode_plus/1,
         decode/1,
         decode_plus/1]).

-include("urilib.hrl").

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-spec build(#uri{} | #url{}) -> string().
%% @spec build(Value) -> URI
%% where
%%    Value = #uri{} | #url{}
%%    URI = string()
%% @doc Returns a URI from the record passed in.
%%
%% @end
build(#uri{scheme=Scheme, userinfo=UserInfo, authority=Authority,
           path=Path, query=QArgs, fragment=Fragment}) ->
  U1 = url_add_scheme(Scheme),
  U2 = url_maybe_add_user(UserInfo, U1),
  U3 = url_add_host_and_port(Scheme,
                             Authority#authority.host,
                             Authority#authority.port, U2),
  U4 = url_add_path(Path, U3),
  U5 = url_maybe_add_qargs(QArgs, U4),
  url_maybe_add_fragment(Fragment, U5).


-spec parse_uri(string()) -> #uri{}.
%% @spec parse_uri(URI) -> ParsedURI.
%% where
%%    URI = string()
%%    ParsedURI = #uri{}
%% @doc Parse a URI string returning the parsed data as a record
%% @end
parse_uri(URI) ->
    case http_uri:parse(URI, [{scheme_defaults, http_uri:scheme_defaults()}, {fragment, true}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query, Fragment}} ->
            #uri{scheme=Scheme,
                 userinfo=parse_userinfo(UserInfo),
                 authority=#authority{host=Host,
                                      port=Port},
                 path=Path,
                 query=parse_query(Query),
                 fragment=Fragment};
        {error, Reason} -> {error, Reason}
    end.


-spec parse_url(string()) -> #url{}.
%% @spec parse_url(URL) -> ParsedURL.
%% where
%%    URI = string()
%%    ParsedURL = #url{}
%% @doc Parse a URL string returning the parsed data as a record
%% @end
parse_url(URL) ->
    case http_uri:parse(URL, [{scheme_defaults, http_uri:scheme_defaults()}, {fragment, true}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query, Fragment}} ->
            User = parse_userinfo(UserInfo),
            #url{scheme=Scheme,
                 username=User#userinfo.username,
                 password=User#userinfo.password,
                 host=Host,
                 port=Port,
                 path=Path,
                 query=parse_query(Query),
                 fragment=Fragment};
        {error, Reason} -> {error, Reason}
    end.


-spec encode(string()) -> string().
%% @spec encode(Value) -> EncodedValue
%% where
%%    Value = string()
%%    EncodedValue = string()
%% @doc Percent encode a string value.
%% @end
encode(Value) ->
    edoc_lib:escape_uri(Value).


-spec encode_plus(string()) -> string().
%% @spec encode_plus(Value) -> EncodedValue
%% where
%%    Value = string()
%%    EncodedValue = string()
%% @doc Percent encode a string value similar to encode/1, but encodes spaces with a
%% plus (+) instead of %20. This function can be used for encoding query arguments.
%%
%% Note: The use of plus for space is defined in RFC-1630 but does not appear
%%       in RFC-3986.
%% @end
encode_plus(Value) ->
    string:join([edoc_lib:escape_uri(V) || V <- string:tokens(Value, " ")], "+").


%% @spec decode(Value) -> DecodedValue
%% where
%%    Value = string()
%%    DecodeValue = string()
%% @doc Decode a percent encoded string value.
%% @end
-spec decode(string()) -> string().
decode(Value) ->
    http_uri:decode(Value).


-spec decode_plus(string()) -> string().
%% @spec decode_plus(Value) -> DecodedValue
%% where
%%    Value = string()
%%    DecodeValue = string()
%% @doc Decode a percent encoded string value that uses pluses for spaces.
%%
%% Note: The use of plus for space is defined in RFC-1630 but does not appear
%%       in RFC-3986.
%% @end
decode_plus(Value) ->
    string:join([http_uri:decode(V) || V <- string:tokens(Value, "+")], " ").


-spec parse_query(string()) -> [].
%% @private
parse_query([]) -> [];
parse_query(Query) ->
    case re:split(Query, "[&|?]", [{return, list}]) of
        [""]    -> [];
        QArgs -> [split_query_arg(Arg) || Arg <- QArgs, Arg =/= []]
    end.


-spec parse_userinfo(string()) -> #userinfo{}.
%% @private
parse_userinfo([]) -> #userinfo{};
parse_userinfo(Value) ->
    case string:tokens(Value, ":") of
        [User, Password] -> #userinfo{username=User, password=Password};
        [User] -> #userinfo{username=User}
    end.


-spec split_query_arg(string()) -> {string(), string()}.
%% @private
split_query_arg(Argument) ->
    [K, V] = string:tokens(Argument, "="),
    {K, V}.


%% @private
url_add_scheme(Scheme) ->
  string:concat(atom_to_list(Scheme), "://").


%% @private
url_maybe_add_user([], URL) -> URL;
url_maybe_add_user(User, URL) ->
  string:concat(URL, string:concat(User, "@")).


%% @private
url_add_host_and_port(http, Host, 80, URL) ->
  string:concat(URL, Host);


%% @private
url_add_host_and_port(https, Host, 443, URL) ->
  string:concat(URL, Host);
url_add_host_and_port(_, Host, Port, URL) ->
  string:concat(URL, string:join([Host, integer_to_list(Port)], ":")).


%% @private
url_add_path(Path, URL) ->
  Escaped = string:join([edoc_lib:escape_uri(P) || P <- string:tokens(Path, "/")], "/"),
  string:join([URL, Escaped], "/").


%% @private
url_maybe_add_qargs([], URL) -> URL;
url_maybe_add_qargs(QArgs, URL) ->
  QStr = string:join([string:join([encode_plus(K), encode_plus(V)], "=") || {K,V} <- QArgs], "&"),
  string:join([URL, QStr], "?").


%% @private
url_maybe_add_fragment([], URL) -> URL;
url_maybe_add_fragment(Value, URL) ->
  Fragment = case string:left(Value, 1) of
    "#" -> edoc_lib:escape_uri(string:sub_string(Value, 2));
    _ -> edoc_lib:escape_uri(Value)
  end,
  string:join([URL, Fragment], "#").
