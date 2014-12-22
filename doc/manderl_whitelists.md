

# Module manderl_whitelists #
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
Adds an email to your email rejection whitelist.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Removes an email address from the whitelist.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Retrieves your email rejection whitelist.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Adds an email to your email rejection whitelist. If the address is currently on your blacklist, that blacklist entry will be removed automatically.


see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Removes an email address from the whitelist.


see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=delete)
<a name="list-1"></a>

### list/1 ###


<pre><code>
list(Params::#{}) -&gt; #{}
</code></pre>
<br />



Retrieves your email rejection whitelist. You can provide an email address or search prefix to limit the results. Returns up to 1000 results.


see [Mandrill documentation](https://mandrillapp.com/api/docs/whitelists.JSON.html#method=list)
