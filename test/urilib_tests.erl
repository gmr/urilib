-module(urilib_tests).

-include_lib("eunit/include/eunit.hrl").


decode_test() ->
    Value = "foo%2fbar%20baz",
    Expect = "foo/bar baz",
    ?assertEqual(Expect, urilib:decode(Value)).

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
