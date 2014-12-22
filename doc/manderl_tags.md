

# Module manderl_tags #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2014 Finexkap

Manderl is a module for interacting with the Mandrill API

## Configuration

* `endpoint :: string()` : Mandrill endpoint (default: `"https://mandrillapp.com/api/1.0/"`)
* `api_key :: string()` : Mandrill API key

## Usage

All function take a map and return a map. Only the <code>key</code> parameter can be ommited, since it is added by the module.


__Authors:__ Gregoire Lejeune ([`gl@finexkap.com`](mailto:gl@finexkap.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_time_series-0">all_time_series/0</a></td><td>
Return the recent history (hourly stats for the last 30 days) for all tags.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Deletes a tag permanently.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Return more detailed information about a single tag, including aggregates of recent stats.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Return all of the user-defined tag information.</td></tr><tr><td valign="top"><a href="#time_series-1">time_series/1</a></td><td>
Return the recent history (hourly stats for the last 30 days) for a tag.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_time_series-0"></a>

### all_time_series/0 ###


<pre><code>
all_time_series() -&gt; #{}
</code></pre>
<br />



Return the recent history (hourly stats for the last 30 days) for all tags


see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=all-time-series)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Deletes a tag permanently. Deleting a tag removes the tag from any messages that have been sent, and also deletes the tag's stats. There is no way to undo this operation, so use it carefully.


see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=delete)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return more detailed information about a single tag, including aggregates of recent stats


see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=info)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Return all of the user-defined tag information


see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=list)
<a name="time_series-1"></a>

### time_series/1 ###


<pre><code>
time_series(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return the recent history (hourly stats for the last 30 days) for a tag


see [Mandrill documentation](https://mandrillapp.com/api/docs/tags.JSON.html#method=time-series)
