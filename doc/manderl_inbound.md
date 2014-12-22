

# Module manderl_inbound #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_domain-1">add_domain/1</a></td><td>
Add an inbound domain to your account.</td></tr><tr><td valign="top"><a href="#add_route-1">add_route/1</a></td><td>
Add a new mailbox route to an inbound domain.</td></tr><tr><td valign="top"><a href="#check_domain-1">check_domain/1</a></td><td>
Check the MX settings for an inbound domain.</td></tr><tr><td valign="top"><a href="#delete_domain-1">delete_domain/1</a></td><td>
Delete an inbound domain from the account.</td></tr><tr><td valign="top"><a href="#delete_route-1">delete_route/1</a></td><td>
Delete an existing inbound mailbox route.</td></tr><tr><td valign="top"><a href="#domains-0">domains/0</a></td><td>
List the domains that have been configured for inbound delivery.</td></tr><tr><td valign="top"><a href="#routes-1">routes/1</a></td><td>
List the mailbox routes defined for an inbound domain.</td></tr><tr><td valign="top"><a href="#send_raw-1">send_raw/1</a></td><td>
Take a raw MIME document destined for a domain with inbound domains set up, and send it to the inbound hook exactly as if it had been sent over SMTP.</td></tr><tr><td valign="top"><a href="#update_route-1">update_route/1</a></td><td>
Update the pattern or webhook of an existing inbound mailbox route.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_domain-1"></a>

### add_domain/1 ###


<pre><code>
add_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add an inbound domain to your account


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=add-domain)
<a name="add_route-1"></a>

### add_route/1 ###


<pre><code>
add_route(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a new mailbox route to an inbound domain


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=add-route)
<a name="check_domain-1"></a>

### check_domain/1 ###


<pre><code>
check_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Check the MX settings for an inbound domain. The domain must have already been added with the add-domain call


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=check-domain)
<a name="delete_domain-1"></a>

### delete_domain/1 ###


<pre><code>
delete_domain(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete an inbound domain from the account. All mail will stop routing for this domain immediately.


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=delete-domain)
<a name="delete_route-1"></a>

### delete_route/1 ###


<pre><code>
delete_route(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete an existing inbound mailbox route


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=delete-route)
<a name="domains-0"></a>

### domains/0 ###


<pre><code>
domains() -&gt; #{}
</code></pre>
<br />



List the domains that have been configured for inbound delivery


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=domains)
<a name="routes-1"></a>

### routes/1 ###


<pre><code>
routes(Params::#{}) -&gt; #{}
</code></pre>
<br />



List the mailbox routes defined for an inbound domain


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=routes)
<a name="send_raw-1"></a>

### send_raw/1 ###


<pre><code>
send_raw(Params::#{}) -&gt; #{}
</code></pre>
<br />



Take a raw MIME document destined for a domain with inbound domains set up, and send it to the inbound hook exactly as if it had been sent over SMTP


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=send-raw)
<a name="update_route-1"></a>

### update_route/1 ###


<pre><code>
update_route(Params::#{}) -&gt; #{}
</code></pre>
<br />



Update the pattern or webhook of an existing inbound mailbox route. If null is provided for any fields, the values will remain unchanged.


see [Mandrill documentation](https://mandrillapp.com/api/docs/inbound.JSON.html#method=update-route)
