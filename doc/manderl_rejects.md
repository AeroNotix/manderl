

# Module manderl_rejects #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td>
Adds an email to your email rejection blacklist.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Deletes an email rejection.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Retrieves your email rejection blacklist.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Adds an email to your email rejection blacklist. Addresses that you add manually will never expire and there is no reputation penalty for removing them from your blacklist. Attempting to blacklist an address that has been whitelisted will have no effect.


see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Deletes an email rejection. There is no limit to how many rejections you can remove from your blacklist, but keep in mind that each deletion has an affect on your reputation.


see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=delete)
<a name="list-1"></a>

### list/1 ###


<pre><code>
list(Params::#{}) -&gt; #{}
</code></pre>
<br />



Retrieves your email rejection blacklist. You can provide an email address to limit the results. Returns up to 1000 results. By default, entries that have expired are excluded from the results; set include_expired to true to include them.


see [Mandrill documentation](https://mandrillapp.com/api/docs/rejects.JSON.html#method=list)
