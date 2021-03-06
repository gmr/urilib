%% =============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016
%% @doc urilib is a RFC-3986 URI Library for Erlang
%% @end
%% =============================================================================
-module(urilib).

-export([build/1,
         parse/1,
         parse/2,
         percent_decode/1,
         percent_encode/1,
         percent_encode/2,
         plus_decode/1,
         plus_encode/1,
         plus_encode/2]).

-export_type([scheme/0,
              host/0,
              username/0,
              password/0,
              userinfo/0,
              authority/0,
              path/0,
              query/0,
              fragment/0,
              uri/0,
              url/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-type hexcase() :: uppercase | lowercase.
-type scheme() :: http | https | atom().
-type host() :: string().
-type username() :: string() | undefined.
-type password() :: string() | undefined.
-type userinfo() :: {username(), password()} | undefined.
-type authority() :: {userinfo(), host(), inet:port_number()}.
-type path() :: string().
-type query() :: [tuple() | string()] | undefined.
-type fragment() :: string() | undefined.
-type uri() :: {scheme(), authority(), path(), query(), fragment()}.
-type url() :: {scheme(), username(), password(), host(), inet:port_number(), path(), query(), fragment()}.

-spec build(Value :: uri() | url()) -> string().
%% @doc Build a URI
%% @end
build({Scheme, {undefined, Host, Port}, Path, Query, Fragment}) ->
    build({Scheme, {{undefined, undefined}, Host, Port}, Path, Query, Fragment});

build({Scheme, {{Username, Password}, Host, Port}, Path, Query, Fragment}) ->
    U1 = url_add_scheme(Scheme),
    U2 = url_maybe_add_userinfo(Username, Password, U1),
    U3 = url_add_host_and_port(Scheme, Host, Port, U2),
    U4 = url_add_path(Path, U3),
    U5 = url_maybe_add_qargs(Query, U4),
    url_maybe_add_fragment(Fragment, U5);

build({Scheme, undefined, Host, Port, Path, Query, Fragment}) ->
    build({Scheme, undefined, undefined, Host, Port, Path, Query, Fragment});

build({Scheme, Username, Password, Host, Port, Path, Query, Fragment}) ->
    U1 = url_add_scheme(Scheme),
    U2 = url_maybe_add_userinfo(Username, Password, U1),
    U3 = url_add_host_and_port(Scheme, Host, Port, U2),
    U4 = url_add_path(Path, U3),
    U5 = url_maybe_add_qargs(Query, U4),
    url_maybe_add_fragment(Fragment, U5).


-spec parse(string()) -> uri() | {error, Reason :: term()}.
%% @doc Parse a URI
%%
%% Returns either the URI or an error term.
%%
%% @end
parse(Value) ->
    case http_uri:parse(Value, [{scheme_defaults, http_uri:scheme_defaults()}, {fragment, true}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query, Fragment}} ->
            {Scheme, {parse_userinfo(UserInfo), Host, Port}, Path, parse_query(Query), parse_fragment(Fragment)};
        {error, Reason} ->
            {error, Reason}
    end.


-spec parse(string(), Return :: uri | url) -> uri() | {error, Reason :: term()}.
%% @doc Parse a URI, returning the result as either a {@type uri()} or {@type url()}.
%%
%% On error, return an error term.
%%
%% @end
parse(Value, uri) ->
    parse(Value);

parse(Value, url) ->
    case http_uri:parse(Value, [{scheme_defaults, http_uri:scheme_defaults()}, {fragment, true}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query, Fragment}} ->
            {Username, Password} = parse_userinfo(UserInfo),
            {Scheme, Username, Password, Host, Port, Path, parse_query(Query), parse_fragment(Fragment)};
        {error, Reason} ->
            {error, Reason}
    end.


-spec percent_encode(string()) -> string().
%% @doc Percent encode a string value. Note that this will return hexidecimal
%% values in lowercase. If you need uppercase values, invoke percent_encode/2
%% with the second parameter as the value ``upercase``.
%% @end
percent_encode(Value) ->
    edoc_lib:escape_uri(Value).


-spec percent_encode(string(), hexcase()) -> string().
%% @doc Percent encode a string value.
%%
%% When lowercase is passed, hexidecimal strings with A-F values in them are returned
%% as lowercase. Likewise, the uppercase value will encode hexidecimal strings as
%% uppercase values.
%% @end
percent_encode(Value, lowercase) ->
    percent_encode(Value);
percent_encode(Value, uppercase) ->
    hex_to_upper(percent_encode(Value)).


-spec percent_decode(string()) -> string().
%% @doc Decode a percent encoded string value.
%% @end
percent_decode(Value) ->
    http_uri:decode(Value).


-spec plus_encode(string()) -> string().
%% @doc Percent encode a string value similar to encode/1, but encodes spaces with a
%% plus (`+') instead of `%20'. This function can be used for encoding query arguments.
%%
%% Note: The use of plus for space is defined in RFC-1630 but does not appear in RFC-3986.
%% @end
plus_encode(Value) ->
    string:join([edoc_lib:escape_uri(V) || V <- string:tokens(Value, " ")], "+").


-spec plus_encode(string(), hexcase()) -> string().
%% @doc Percent encode a string value similar to encode/1, but encodes spaces with a
%% plus (`+') instead of `%20'. This function can be used for encoding query arguments.
%% When lowercase is passed, hexidecimal strings with A-F values in them are returned
%% as lowercase. Likewise, the uppercase value will encode hexidecimal strings as
%% uppercase values.
%%
%% Note: The use of plus for space is defined in RFC-1630 but does not appear in RFC-3986.
%% @end
plus_encode(Value, lowercase) ->
    plus_encode(Value);
plus_encode(Value, uppercase) ->
    hex_to_upper(plus_encode(Value)).


-spec plus_decode(string()) -> string().
%% @doc Decode a percent encoded string value that uses pluses for spaces.
%%
%% Note: The use of plus for space is defined in RFC-1630 but does not appear
%%       in RFC-3986.
%% @end
plus_decode(Value) ->
    string:join([http_uri:decode(V) || V <- string:tokens(Value, "+")], " ").


%% Private Functions

-spec parse_fragment(string()) -> string() | undefined.
%% @private
parse_fragment([]) ->
    undefined;

parse_fragment(Value) ->
    Value.


-spec parse_query(string()) -> [tuple() | string()] | undefined.
%% @private
parse_query(Query) ->
    QArgs = re:split(Query, "[&|?]", [{return, list}]),
    parse_query_result([split_query_arg(Arg) || Arg <- QArgs, Arg =/= []]).


-spec parse_query_result(string()) -> [tuple() | string()] | undefined.
%% @private
parse_query_result([]) ->
    undefined;

parse_query_result(QArgs) ->
    QArgs.


-spec parse_userinfo(string()) -> userinfo().
%% @private
parse_userinfo(Value) ->
    parse_userinfo_result(string:tokens(Value, ":")).


-spec parse_userinfo_result(list()) -> userinfo().
%% @private
parse_userinfo_result([User, Password]) ->
    {User, Password};

parse_userinfo_result([User]) ->
    {User, undefined};

parse_userinfo_result([]) ->
    undefined.


-spec split_query_arg(string()) -> {string(), string()} | undefined.
%% @private
split_query_arg(Argument) ->
    case string:tokens(Argument, "=") of
        [K, V] -> {plus_decode(K), plus_decode(V)};
        [Value] -> plus_decode(Value)
    end.


-spec url_add_scheme(atom()) -> string().
%% @private
url_add_scheme(undefined) ->
    "http://";

url_add_scheme(Scheme) ->
    string:concat(atom_to_list(Scheme), "://").


-spec url_maybe_add_userinfo(username(), password(), string()) -> string().
%% @private
url_maybe_add_userinfo([], [], URL) ->
    URL;

url_maybe_add_userinfo(undefined, undefined, URL) ->
    URL;

url_maybe_add_userinfo(Username, [], URL) ->
    url_maybe_add_userinfo(Username, undefined, URL);

url_maybe_add_userinfo(Username, undefined, URL) ->
    string:concat(URL, string:concat(Username, "@"));

url_maybe_add_userinfo(Username, Password, URL) ->
    string:concat(URL, string:concat(string:join([Username, Password], ":"), "@")).


-spec url_add_host_and_port(scheme(), host(), inet:port_number(), string()) -> string().
%% @private
url_add_host_and_port(undefined, Host, undefined, URL) ->
    string:concat(URL, Host);

url_add_host_and_port(http, Host, undefined, URL) ->
    string:concat(URL, Host);

url_add_host_and_port(http, Host, 80, URL) ->
    string:concat(URL, Host);

url_add_host_and_port(https, Host, undefined, URL) ->
    string:concat(URL, Host);

url_add_host_and_port(https, Host, 443, URL) ->
    string:concat(URL, Host);

url_add_host_and_port(_, Host, Port, URL) ->
    string:concat(URL, string:join([Host, integer_to_list(Port)], ":")).


-spec url_add_path(path(), string()) -> string().
%% @private
url_add_path(undefined, URL) ->
    string:concat(URL, "/");

url_add_path(Path, URL) ->
    Escaped = string:join([url_escape_path_segment(P) || P <- string:tokens(Path, "/")], "/"),
    Joined = string:join([URL, Escaped], "/"),
    case Path /= "/" andalso lists:suffix("/", Path) of
        true -> string:concat(Joined, "/");
        false -> Joined
    end.


-spec url_escape_path_segment(string()) -> string().
%% @private
url_escape_path_segment(Value) ->
    edoc_lib:escape_uri(http_uri:decode(Value)).


-spec url_maybe_add_qargs(query(), string()) -> string().
%% @private
url_maybe_add_qargs(undefined, URL) ->
    URL;

url_maybe_add_qargs([], URL) ->
    URL;

url_maybe_add_qargs(QArgs, URL) ->
    QStr = string:join([url_maybe_encode_query_arg(Arg) || Arg <- QArgs], "&"),
    string:join([URL, QStr], "?").


-spec url_maybe_encode_query_arg(tuple() | string()) -> string().
%% @private
url_maybe_encode_query_arg({K, V}) ->
    string:join([plus_encode(K), plus_encode(V)], "=");

url_maybe_encode_query_arg(V) ->
    plus_encode(V).


-spec url_maybe_add_fragment(fragment(), string()) -> string().
%% @private
url_maybe_add_fragment(undefined, URL) -> URL;
url_maybe_add_fragment([], URL) -> URL;
url_maybe_add_fragment(Value, URL) ->
    Fragment = case string:left(Value, 1) of
        "#" -> edoc_lib:escape_uri(string:sub_string(Value, 2));
        _ -> edoc_lib:escape_uri(Value)
    end,
    string:join([URL, Fragment], "#").


-spec hex_to_upper(string()) -> string().
%% @private
hex_to_upper(Value) ->
    hex_to_upper(Value, []).
hex_to_upper([], Accum) ->
    lists:reverse(Accum);
hex_to_upper([$%, B1, B2|T], Accum) ->
    hex_to_upper(T, [to_upper(B2), to_upper(B1), $% | Accum]);
hex_to_upper([H|T], Accum) ->
    hex_to_upper(T, [H | Accum]).


-spec to_upper(char()) -> char().
to_upper($a) -> $A;
to_upper($b) -> $B;
to_upper($c) -> $C;
to_upper($d) -> $D;
to_upper($e) -> $E;
to_upper($f) -> $F;
to_upper(C) -> C.
