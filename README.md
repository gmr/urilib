urilib
======
[RFC-3986](https://tools.ietf.org/html/rfc3986) URI Library for Erlang.

[![Build Status](https://travis-ci.org/gmr/urilib.svg?branch=master)](https://travis-ci.org/gmr/urilib) [![codecov.io](https://codecov.io/github/gmr/urilib/coverage.svg?branch=master)](https://codecov.io/github/gmr/urilib?branch=master)

Example Usage
-------------
```erlang
-include_lib("urilib.h").

URI = urilib:parse("http://foo:bar@www.google.com/search?baz=qux#corgie"),
io:format("Parsed URI: ~p~n", [URI]).

URL = urllib:build({http, undefined, undefined, "www.google.com", undefined, "/search", [{"foo", "bar"}], "baz"}),
io:format("Built URL: ~s~n", [URL]).
```
