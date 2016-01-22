urilib
======
[RFC-3986](https://tools.ietf.org/html/rfc3986) URI Library for Erlang.

Parse and build URIs with automatic percent encoding and plus encoding of query arguments.

[![Version](https://img.shields.io/hexpm/v/urilib.svg)][![Downloads](https://img.shields.io/hexpm/dt/urilib.svg)][![Build Status](https://travis-ci.org/gmr/urilib.svg?branch=master)](https://travis-ci.org/gmr/urilib) [![codecov.io](https://codecov.io/github/gmr/urilib/coverage.svg?branch=master)](https://codecov.io/github/gmr/urilib?branch=master)

## API

### Functions

Function           | Description
------------------ | ------------------
`build/1`          | Build a URI from a `uri()` or `url`.
`parse/1`          | Parse a URI from a string, returning a `uri()`.
`parse/2`          | Parse a URI, returning the result as either a `uri()` or `url()`.
`percent_decode/1` | Decode a percent encoded string value.
`percent_encode/1` | Percent encode a string value.
`plus_decode/1`    | Decode a percent encoded string value that uses pluses for spaces.
`plus_encode/1`    | Percent encode a string value similar to encode/1, but encodes spaces with a plus (`+`) instead of `%20`.

### Types

Type          | Definition
------------- | ----------------------
`scheme()`    | `http | https | atom()`
`username()`  | `string() | undefined`
`password()`  | `string() | undefined`
`userinfo()`  | `{username(), password()} | undefined`
`host()`      | `string()`
`tcp_port()`  | `integer()`
`authority()` | `{userinfo(), host(), tcp_port()}`
`path()`      | `string()`
`query()`     | `[tuple() | string()] | undefined`
`fragment()`  | `string() | undefined`
`uri()`       | `{scheme(), authority(), path(), query(), fragment()}`
`url()`       | `{scheme(), username(), password(), host(), tcp_port(), path(), query(), fragment()}`

## Example Usage

```erlang
Eshell V7.2.1  (abort with ^G)
1> urilib:build({http, {{"guest", "guest"}, "localhost", 15672}, "/api/queues", [{"name", "test"}], undefined}).
"http://guest:guest@localhost:15672/api/queues?name=test"

2> urilib:build({http, "guest", "guest", "localhost", 15672, "/api/queues", [{"name", "test"}], undefined}).    
"http://guest:guest@localhost:15672/api/queues?name=test"

3> urilib:parse("http://guest:guest@localhost:15672/api/queues?name=test").
{http,{{"guest","guest"},"localhost",15672},
      "/api/queues",
      [{"name","test"}],
      undefined}
```
