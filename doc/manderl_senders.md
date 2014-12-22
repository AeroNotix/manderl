

# Module manderl_senders #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_domain-1">add_domain/1</a></td><td>see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=add-domain).</td></tr><tr><td valign="top"><a href="#check_domain-1">check_domain/1</a></td><td>
Checks the SPF and DKIM settings for a domain.</td></tr><tr><td valign="top"><a href="#domains-0">domains/0</a></td><td>
Returns the sender domains that have been added to this account.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Return more detailed information about a single sender, including aggregates of recent stats.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Return the senders that have tried to use this account.</td></tr><tr><td valign="top"><a href="#time_series-1">time_series/1</a></td><td>
Return the recent history (hourly stats for the last 30 days) for a sender.</td></tr><tr><td valign="top"><a href="#verify_domain-1">verify_domain/1</a></td><td>
Sends a verification email in order to verify ownership of a domain.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_domain-1"></a>

### add_domain/1 ###


<pre><code>
add_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />

see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=add-domain)
<a name="check_domain-1"></a>

### check_domain/1 ###


<pre><code>
check_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Checks the SPF and DKIM settings for a domain. If you haven't already added this domain to your account, it will be added automatically.


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=check-domain)
<a name="domains-0"></a>

### domains/0 ###


<pre><code>
domains() -&gt; #{}
</code></pre>
<br />



Returns the sender domains that have been added to this account.


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=domains)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return more detailed information about a single sender, including aggregates of recent stats


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=info)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Return the senders that have tried to use this account.


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=list)
<a name="time_series-1"></a>

### time_series/1 ###


<pre><code>
time_series(Params::#{}) -&gt; #{}
</code></pre>
<br />



Return the recent history (hourly stats for the last 30 days) for a sender


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=time-series)
<a name="verify_domain-1"></a>

### verify_domain/1 ###


<pre><code>
verify_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Sends a verification email in order to verify ownership of a domain. Domain verification is an optional step to confirm ownership of a domain. Once a domain has been verified in a Mandrill account, other accounts may not have their messages signed by that domain unless they also verify the domain. This prevents other Mandrill accounts from sending mail signed by your domain.


see [Mandrill documentation](https://mandrillapp.com/api/docs/senders.JSON.html#method=verify-domain)
