

# Module manderl_users #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#info-0">info/0</a></td><td>
Return the information about the API-connected user.</td></tr><tr><td valign="top"><a href="#ping-0">ping/0</a></td><td>
Validate an API key and respond to a ping.</td></tr><tr><td valign="top"><a href="#senders-0">senders/0</a></td><td>
Return the senders that have tried to use this account, both verified and unverified.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="info-0"></a>

### info/0 ###


<pre><code>
info() -&gt; #{}
</code></pre>
<br />



Return the information about the API-connected user


see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=info)
<a name="ping-0"></a>

### ping/0 ###


<pre><code>
ping() -&gt; #{}
</code></pre>
<br />



Validate an API key and respond to a ping


see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=ping2)
<a name="senders-0"></a>

### senders/0 ###


<pre><code>
senders() -&gt; #{}
</code></pre>
<br />



Return the senders that have tried to use this account, both verified and unverified


see [Mandrill documentation](https://mandrillapp.com/api/docs/users.JSON.html#method=senders)
