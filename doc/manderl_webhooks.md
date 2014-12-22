

# Module manderl_webhooks #
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
Add a new webhook.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Delete an existing webhook.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Given the ID of an existing webhook, return the data about it.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Get the list of all webhooks defined on the account.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>
Update an existing webhook.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a new webhook


see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete an existing webhook


see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=delete)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Given the ID of an existing webhook, return the data about it


see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=info)
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; #{}
</code></pre>
<br />



Get the list of all webhooks defined on the account


see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=list)
<a name="update-1"></a>

### update/1 ###


<pre><code>
update(Params::#{}) -&gt; #{}
</code></pre>
<br />



Update an existing webhook


see [Mandrill documentation](https://mandrillapp.com/api/docs/webhooks.JSON.html#method=update)
