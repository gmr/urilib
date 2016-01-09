-module(urilib_tests).

-include_lib("eunit/include/eunit.hrl").

-include("urilib.hrl").

decode_test() ->
    Value = "foo%2fbar%20baz",
    Expect = "foo/bar baz",
    ?assertEqual(Expect, urilib:decode(Value)).

decode_plus_test() ->
    Value = "foo/bar+baz",
    Expect = "foo/bar baz",
    ?assertEqual(Expect, urilib:decode_plus(Value)).

encode1_test() ->
    Value = "foo/bar baz",
    Expect = "foo%2fbar%20baz",
    ?assertEqual(Expect, urilib:encode(Value)).

encode1_unicode_test() ->
    Value = "foo/bar✈baz",
    Expect = "foo%2fbar%c0%88baz",
    ?assertEqual(Expect, urilib:encode(Value)).

encode_plus1_test() ->
    Value = "foo/bar baz",
    Expect = "foo%2fbar+baz",
    ?assertEqual(Expect, urilib:encode_plus(Value)).

parse_uri_variation1_test() ->
    URI = "amqp://guest:rabbitmq@rabbitmq:5672/%2f?heartbeat=5",
    Expect = #uri{scheme=amqp,
                  userinfo=#userinfo{username="guest",
                                     password="rabbitmq"},
                  authority=#authority{host="rabbitmq", port=5672},
                  path="/%2f",
                  query=[{"heartbeat", "5"}],
                  fragment=[]},
    ?assertEqual(Expect, urilib:parse_uri(URI)).

parse_uri_variation2_test() ->
    URI = "http://www.google.com/search?foo=bar#baz",
    Expect = #uri{scheme=http,
                  userinfo=#userinfo{},
                  authority=#authority{host="www.google.com", port=80},
                  path="/search",
                  query=[{"foo", "bar"}],
                  fragment="#baz"},
    ?assertEqual(Expect, urilib:parse_uri(URI)).

parse_uri_variation3_test() ->
    URI = "https://www.google.com/search",
    Expect = #uri{scheme=https,
                  userinfo=#userinfo{},
                  authority=#authority{host="www.google.com", port=443},
                  path="/search",
                  query=[],
                  fragment=[]},
    ?assertEqual(Expect, urilib:parse_uri(URI)).

parse_url_variation1_test() ->
    URL = "amqp://guest:rabbitmq@rabbitmq:5672/%2f?heartbeat=5",
    Expect = #url{scheme=amqp,
                  username="guest",
                  password="rabbitmq",
                  host="rabbitmq",
                  port=5672,
                  path="/%2f",
                  query=[{"heartbeat", "5"}],
                  fragment=[]},
    ?assertEqual(Expect, urilib:parse_url(URL)).

parse_url_variation2_test() ->
    URL = "http://www.google.com/search?foo=bar#baz",
    Expect = #url{scheme=http,
                  username=undefined,
                  password=undefined,
                  host="www.google.com",
                  port=80,
                  path="/search",
                  query=[{"foo", "bar"}],
                  fragment="#baz"},
    ?assertEqual(Expect, urilib:parse_url(URL)).

parse_url_variation3_test() ->
    URL = "https://www.google.com/search",
    Expect = #url{scheme=https,
                  username=undefined,
                  password=undefined,
                  host="www.google.com",
                  port=443,
                  path="/search",
                  query=[],
                  fragment=[]},
    ?assertEqual(Expect, urilib:parse_url(URL)).
