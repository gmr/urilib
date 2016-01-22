urilib
======
[RFC-3986](https://tools.ietf.org/html/rfc3986) URI Library for Erlang.

[![Build Status](https://travis-ci.org/gmr/urilib.svg?branch=master)](https://travis-ci.org/gmr/urilib) [![codecov.io](https://codecov.io/github/gmr/urilib/coverage.svg?branch=master)](https://codecov.io/github/gmr/urilib?branch=master)

Example Usage
-------------

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
