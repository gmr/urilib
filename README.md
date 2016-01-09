urilib
======
[RFC-3986](https://tools.ietf.org/html/rfc3986) URI Library for Erlang.

[![Build Status](https://travis-ci.org/gmr/urilib.svg?branch=master)](https://travis-ci.org/gmr/urilib)

API
---

<a name="index"></a>

## Function Index ##

<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build-1">build/1</a></td><td>Returns a URI from the record passed in.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decode a percent encoded string value.</td></tr><tr><td valign="top"><a href="#decode_plus-1">decode_plus/1</a></td><td>Decode a percent encoded string value that uses pluses for spaces.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Percent encode a string value.</td></tr><tr><td valign="top"><a href="#encode_plus-1">encode_plus/1</a></td><td>Percent encode a string value similar to encode/1, but encodes spaces with a
plus (+) instead of %20.</td></tr><tr><td valign="top"><a href="#parse_uri-1">parse_uri/1</a></td><td>Parse a URI string returning the parsed data as a record.</td></tr><tr><td valign="top"><a href="#parse_url-1">parse_url/1</a></td><td>Parse a URL string returning the parsed data as a record.</td></tr></table>

<a name="functions"></a>

## Function Details ##

<a name="build-1"></a>

### build/1 ###

<pre><code>
build(Uri::Value) -&gt; URI
</code></pre>

<ul class="definitions"><li><code>Value = #uri{} | #url{}</code></li><li><code>URI = string()</code></li></ul>

Returns a URI from the record passed in.

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Value) -&gt; DecodedValue
</code></pre>

<ul class="definitions"><li><code>Value = string()</code></li><li><code>DecodeValue = string()</code></li></ul>

Decode a percent encoded string value.

<a name="decode_plus-1"></a>

### decode_plus/1 ###

<pre><code>
decode_plus(Value) -&gt; DecodedValue
</code></pre>

<ul class="definitions"><li><code>Value = string()</code></li><li><code>DecodeValue = string()</code></li></ul>

Decode a percent encoded string value that uses pluses for spaces.

Note: The use of plus for space is defined in RFC-1630 but does not appear
in RFC-3986.

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Value) -&gt; EncodedValue
</code></pre>

<ul class="definitions"><li><code>Value = string()</code></li><li><code>EncodedValue = string()</code></li></ul>

Percent encode a string value.

<a name="encode_plus-1"></a>

### encode_plus/1 ###

<pre><code>
encode_plus(Value) -&gt; EncodedValue
</code></pre>

<ul class="definitions"><li><code>Value = string()</code></li><li><code>EncodedValue = string()</code></li></ul>

Percent encode a string value similar to encode/1, but encodes spaces with a
plus (+) instead of %20. This function can be used for encoding query arguments.

Note: The use of plus for space is defined in RFC-1630 but does not appear
in RFC-3986.

<a name="parse_uri-1"></a>

### parse_uri/1 ###

<pre><code>
parse_uri(URI) -&gt; ParsedURI
</code></pre>

<ul class="definitions"><li><code>URI = string()</code></li><li><code>ParsedURI = #uri{}</code></li></ul>

Parse a URI string returning the parsed data as a record

<a name="parse_url-1"></a>

### parse_url/1 ###

<pre><code>
parse_url(URL) -&gt; ParsedURL
</code></pre>

<ul class="definitions"><li><code>URI = string()</code></li><li><code>ParsedURL = #url{}</code></li></ul>

Parse a URL string returning the parsed data as a record

