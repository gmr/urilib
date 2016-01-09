urilib
======
[RFC-3986](https://tools.ietf.org/html/rfc3986) URI Library for Erlang.

[![Build Status](https://travis-ci.org/gmr/urilib.svg?branch=master)](https://travis-ci.org/gmr/urilib) [![codecov.io](https://codecov.io/github/gmr/urilib/coverage.svg?branch=master)](https://codecov.io/github/gmr/urilib?branch=master)

API
---

<a name="build-1"></a>

### build/1 ###
```erlang
build(Uri::Value) -> URI
```
<ul class="definitions"><li><code>Value = #uri{} | #url{}</code></li><li><code>URI = string()</code></li></ul>

Returns a URI from the record passed in.

<a name="decode-1"></a>

### decode/1 ###

```erlang
decode(Value) -> DecodedValue
```

<ul class="definitions"><li><code>Value = string()</code></li><li><code>DecodeValue = string()</code></li></ul>

Decode a percent encoded string value.

<a name="decode_plus-1"></a>

### decode_plus/1 ###

```erlang
decode_plus(Value) -&gt; DecodedValue
```

<ul class="definitions"><li><code>Value = string()</code></li><li><code>DecodeValue = string()</code></li></ul>

Decode a percent encoded string value that uses pluses for spaces.

Note: The use of plus for space is defined in RFC-1630 but does not appear
in RFC-3986.

<a name="encode-1"></a>

### encode/1 ###

```erlang
encode(Value) -&gt; EncodedValue
```

<ul class="definitions"><li><code>Value = string()</code></li><li><code>EncodedValue = string()</code></li></ul>

Percent encode a string value.

<a name="encode_plus-1"></a>

### encode_plus/1 ###

```erlang
encode_plus(Value) -&gt; EncodedValue
```

<ul class="definitions"><li><code>Value = string()</code></li><li><code>EncodedValue = string()</code></li></ul>

Percent encode a string value similar to encode/1, but encodes spaces with a
plus (+) instead of %20. This function can be used for encoding query arguments.

Note: The use of plus for space is defined in RFC-1630 but does not appear
in RFC-3986.

<a name="parse_uri-1"></a>

### parse_uri/1 ###

```erlang
parse_uri(URI) -&gt; ParsedURI
```

<ul class="definitions"><li><code>URI = string()</code></li><li><code>ParsedURI = #uri{}</code></li></ul>

Parse a URI string returning the parsed data as a record

<a name="parse_url-1"></a>

### parse_url/1 ###

```erlang
parse_url(URL) -&gt; ParsedURL
```

<ul class="definitions"><li><code>URI = string()</code></li><li><code>ParsedURL = #url{}</code></li></ul>

Parse a URL string returning the parsed data as a record

