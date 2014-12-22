

# Module manderl_subaccounts #
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
Add a new subaccount.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>
Delete an existing subaccount.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>
Given the ID of an existing subaccount, return the data about it.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Get the list of subaccounts defined for the account, optionally filtered by a prefix.</td></tr><tr><td valign="top"><a href="#pause-1">pause/1</a></td><td>
Pause a subaccount's sending.</td></tr><tr><td valign="top"><a href="#resume-1">resume/1</a></td><td>
Resume a paused subaccount's sending.</td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>
Update an existing subaccount.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###


<pre><code>
add(Params::#{}) -&gt; #{}
</code></pre>
<br />



Add a new subaccount


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=add)
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Params::#{}) -&gt; #{}
</code></pre>
<br />



Delete an existing subaccount. Any email related to the subaccount will be saved, but stats will be removed and any future sending calls to this subaccount will fail.


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=delete)
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Params::#{}) -&gt; #{}
</code></pre>
<br />



Given the ID of an existing subaccount, return the data about it


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=info)
<a name="list-1"></a>

### list/1 ###


<pre><code>
list(Params::#{}) -&gt; #{}
</code></pre>
<br />



Get the list of subaccounts defined for the account, optionally filtered by a prefix


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=list)
<a name="pause-1"></a>

### pause/1 ###


<pre><code>
pause(Params::#{}) -&gt; #{}
</code></pre>
<br />



Pause a subaccount's sending. Any future emails delivered to this subaccount will be queued for a maximum of 3 days until the subaccount is resumed.


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=pause)
<a name="resume-1"></a>

### resume/1 ###


<pre><code>
resume(Params::#{}) -&gt; #{}
</code></pre>
<br />



Resume a paused subaccount's sending


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=resume)
<a name="update-1"></a>

### update/1 ###


<pre><code>
update(Params::#{}) -&gt; #{}
</code></pre>
<br />



Update an existing subaccount


see [Mandrill documentation](https://mandrillapp.com/api/docs/subaccounts.JSON.html#method=update)
