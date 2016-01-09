%% =============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016
%% @doc urilib is a RFC-3986 URI Library for Erlang
%% @end
%% =============================================================================
-module(urilib).

-export([build/1,
         parse/1,
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
build(_URL) ->
    ok.


-spec parse(string()) -> #uri{}.
parse(URL) -> parse_uri(URL).


-spec parse_uri(string()) -> #uri{}.
parse_uri(_URL) ->
    ok.


-spec parse_url(string()) -> #url{}.
parse_url(_URL) ->
    ok.


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
